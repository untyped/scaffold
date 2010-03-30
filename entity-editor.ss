#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "attribute-editor.ss"
         "editor-internal.ss"
         "util.ss"
         "view-common.ss")

; Interfaces -------------------------------------

(define entity-editor<%>
  (interface (editor<%>)
    get-entity
    get-initial-value))

; Mixins -----------------------------------------

(define-mixin entity-editor-mixin (html-element<%>) (entity-editor<%>)
  
  (inherit core-html-attributes)
  
  ; Fields -------------------------------------
  
  ; entity
  (init-field entity #:accessor)
  
  ; (listof attribute)
  (init [auto-attributes (and entity (entity-data-attributes entity))])
  
  ; (listof (cons attribute editor<%>))
  (init-field auto-editors
    (or (and auto-attributes 
             (map (lambda (attribute)
                    (cons attribute (default-attribute-editor attribute)))
                  auto-attributes))
        (error "entity-editor constructor: insufficient arguments"))
    #:accessor)
  
  ; (U snooze-struct #f)
  (cell initial-value #f #:accessor)
  
  ; (cellof (listof check-result))
  (init-cell check-results null #:accessor #:mutator)
  
  ; (listof (U string symbol))
  (init [classes null])
  
  (super-new [classes (list* 'smoke-entity-editor 'ui-widget classes)])
  
  ; Methods ------------------------------------
  
  ; -> (listof (U xml (seed -> xml)))
  (define/augment (get-html-requirements)
    (list* tooltip-script
           snooze-styles
           (inner null get-html-requirements)))
  
  ; -> (listof form-element<%>)
  (define/public (get-form-elements)
    (filter (cut is-a? <> form-element<%>)
            (get-child-components)))
  
  ; -> (listof component<%>)
  (define/override (get-child-components)
    (append (super get-child-components)
            (get-attribute-editors)))
  
  ; -> (listof editor<%>)
  (define/public (get-attribute-editors)
    (map cdr (get-auto-editors)))
  
  ; -> (listof attribute)
  (define/public (get-auto-attributes)
    (map car (get-auto-editors)))
  
  ; seed -> xml
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper seed (render-attributes seed (get-auto-attributes)))))
  
  ; seed xml+quotable -> xml
  (define/public (render-wrapper seed contents)
    (xml (table (@ ,(core-html-attributes seed))
                (tbody ,contents))))
  
  ; seed (listof attribute) -> xml
  (define/public (render-attributes seed attrs)
    (xml ,@(for/list ([attribute (in-list attrs)])
             (render-attribute-label+editor seed attribute))))
  
  ; seed attribute -> xml+quotable
  (define/public (render-attribute-label seed attribute)
    (attribute-label-xml attribute))
  
  ; seed attribute [editor%] -> xml
  (define/public (render-attribute-editor seed attribute [editor (get-attribute-editor attribute)])
    (opt-xml editor ,(send editor render seed)))
  
  ; seed attribute [editor%] -> xml
  (define/public (render-attribute-label+editor seed attribute [editor (get-attribute-editor attribute)])
    (render-label+editor+results
     seed 
     (render-attribute-label  seed attribute)
     (opt-xml editor ,(render-attribute-editor seed attribute editor))
     (filter-check-results (get-check-results) attribute editor)))
  
  ; attribute -> (U editor% #f)
  (define/private (get-attribute-editor attribute)
    (let ([attr+editor (assoc attribute (get-auto-editors))])
      (and attr+editor (cdr attr+editor))))
  
  ; seed xml+quotable xml -> xml+quotable
  (define/public (render-label+editor seed label-xml editor-xml)
    (render-label+editor+results seed label-xml editor-xml null))
  
  ; seed xml+quotable xml (listof check-result) -> xml+quotable
  (define/public (render-label+editor+results seed label-xml editor-xml results)
    (xml (tr (th (@ [class "attribute-label"]) ,label-xml)
             (td (@ [class "attribute-value"]) 
                 ,editor-xml
                 ,(render-check-label seed results)))))
  
  
  ; seed (listof check-result) [boolean] -> xml
  (define/public (render-check-results seed results [tooltip? #t])
    (xml (ul (@ [class ,(if tooltip? 
                            "check-results tooltip"
                            "check-results")])
             ,@(for/list ([result results])
                 (define class (check-result->class result))
                 (xml (li (@ [class ,class])
                          ,(check-result-message result)))))))
  
  ; seed [boolean] -> xml
  (define/public (render-check-label seed reportable-results [tooltip? #t])
    ; (U 'check-success 'check-warning 'check-failure 'check-exn)
    (define class (check-results->class reportable-results))
    ; xml
    (xml (span (@ [class ,(if tooltip? "check-label tooltip-anchor" "check-label")])
               ,(opt-xml (not (eq? class 'check-success))
                  ,(check-result-icon class)
                  ,(render-check-results seed reportable-results tooltip?)))))
  
  ; Value processing -----------------------------
  
  ; -> snooze-struct
  (define/public (get-value)
    (let ([init (get-initial-value)])
      (if (snooze-struct? init)
          (for/fold ([val init])
                    ([attr+editor (in-list (get-auto-editors))])
                    (match-let ([(cons attribute editor) attr+editor])
                      (snooze-struct-set val attribute (send editor get-value))))
          (raise-type-error 'entity-editor.get-value "snooze-struct" #f))))
  
  ; snooze-struct -> void
  (define/public (set-value! val)
    (unless (snooze-struct? val)
      (raise-type-error 'entity-editor.set-value! "snooze-struct" val))
    (web-cell-set! initial-value-cell val)
    (for ([attr+editor (in-list (get-auto-editors))])
      (match-let ([(cons attribute editor) attr+editor])
        (send editor set-value! (snooze-struct-ref val attribute)))))
  
  ; -> boolean
  (define/public (value-changed?)
    (ormap (cut send <> value-changed?)
           (get-form-elements)))
  
  ; -> (listof check-result)
  (define/public (parse)
    (for/append ([form-element (in-list (get-form-elements))])
      (if (send form-element value-valid?)
          null
          (check/annotate ([ann:form-elements form-element])
            (send form-element get-value-error)))))
  
  ; -> (listof check-result)
  (define/public (validate)
    (check-problems (check-snooze-struct (get-value))))
  
  ; -> void
  (define/public (commit-changes)
    (let ([val (get-value)])
      (with-transaction 
          #:metadata (list (if (snooze-struct-saved? val)
                               (format "Created ~a" (format-snooze-struct val))
                               (format "Updated ~a" (format-snooze-struct val))))
        (begin0 (save! val)
                (clear-continuation-table!))))))

; Classes ----------------------------------------

(define entity-editor%
  (entity-editor-mixin html-element%))

; Helpers ----------------------------------------

; attribute (U form-element<%> #f) -> (check-result -> boolean)
(define (make-check-result-filter/attribute+editor attribute editor)
  (if editor
      (lambda (result)
        (or (memq editor (check-result-annotation result ann:form-elements))
            (check-result-has-attribute? result attribute)))
      (lambda (result)
        (check-result-has-attribute? result attribute))))

; (listof check-result) attribute (U form-element% #f) -> (listof check-result)
(define (filter-check-results results attribute editor)
  (filter (make-check-result-filter/attribute+editor attribute editor) results))

; Provide statements -----------------------------

(provide entity-editor<%>
         entity-editor-mixin
         entity-editor%)
