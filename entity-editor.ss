#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "attribute-editor.ss"
         "compound-editor.ss"
         "editor-internal.ss"
         "util.ss"
         "view-common.ss")

; Interfaces -------------------------------------

(define entity-editor<%>
  (interface (editor<%> form-element<%>)
    get-entity
    get-initial-value
    set-check-results!
    get-check-results))

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
    (render-wrapper seed (render-attributes seed (get-auto-attributes))))
  
  ; seed xml+quotable -> xml
  (define/public (render-wrapper seed contents)
    (xml (table (@ ,(core-html-attributes seed))
                (tbody ,contents))))
  
  ; seed (listof attribute) -> xml
  (define/public (render-attributes seed attrs)
    (xml ,@(for/list ([attribute (in-list attrs)])
             (render-attribute seed attribute))))
  
  ; seed attribute [editor%] -> xml
  (define/public (render-attribute seed
                                   attribute
                                   #:label  [label  (render-attribute-label seed attribute)]
                                   #:editor [editor (get-attribute-editor attribute)])
    (unless label
      (error (format "entity-editor.render-attribute-editor: No label specified for attribute: ~a" attribute)))
    (unless editor
      (error (format "entity-editor.render-attribute-editor: No editor specified for attribute: ~a" attribute)))
    (render-label+editor+results seed label editor (get-attribute-results attribute editor)))
  
  ; seed attribute -> xml+quotable
  (define/public (render-attribute-label seed attribute)
    (xml ,(attribute-label-xml attribute) 
         ,(opt-xml (not (type-allows-null? (attribute-type attribute))) ,(default-required-label))))
  
  ; seed attribute [editor%] -> xml
  (define/public (render-attribute-editor seed attribute [editor (get-attribute-editor attribute)])
    (unless editor
      (error (format "entity-editor.render-attribute-editor: No editor specified for attribute: ~a" attribute)))
    (send editor render seed))
  
  ; seed attribute [(listof check-result)] -> xml
  (define/public (render-attribute-results seed attribute [results (get-attribute-results attribute)])
    (unless results
      (error (format "entity-editor.render-attribute-results: No results specified for attribute: ~a" attribute)))
    (render-check-label seed results))
  
  ; seed (listof attribute) [(listof check-result)] -> xml
  (define/public (render-attributes-results seed attributes [results (get-attributes-results attributes)])
    (unless results
      (error (format "entity-editor.render-attributes-results: No results specified for attributes: ~a" attributes)))
    (render-check-label seed results))
  
  ; attribute -> (U editor% #f)
  (define/public (get-attribute-editor attribute)
    (let ([attr+editor (assoc attribute (get-auto-editors))])
      (and attr+editor (cdr attr+editor))))
  
  ; attribute -> (listof check-result)
  (define/public (get-attribute-results attribute [editor (get-attribute-editor attribute)])
    (filter-results (get-check-results) (list attribute) editor))
  
  ; (listof attribute) -> (listof check-result)
  (define/public (get-attributes-results attributes [editor (get-attribute-editor attributes)])
    (filter-results (get-check-results) attributes editor))
  
  ; form-element<%> -> (listof check-result)
  (define/public (get-editor-results editor)
    (filter-results/editor (get-check-results) editor))
  
  ; seed xml+quotable html-component<%> -> xml
  (define/public (render-label+editor seed label-xml editor)
    (render-label+editor+results seed label-xml editor (get-editor-results editor)))
  
  ; seed xml+quotable html-component<%> (listof check-result) -> xml
  (define/public (render-label+editor+results seed label-xml editor results)
    (render-label+value seed label-xml (xml ,(send editor render seed) ,(render-check-label seed results))))
  
  ; seed xml+quotable xml -> xml
  (define/public (render-label+value seed label-xml value-xml)
    (xml (tr (th (@ [class "attribute-label"]) ,label-xml)
             (td (@ [class "attribute-value"]) ,value-xml))))
  
  ; seed [boolean] -> xml
  (define/public (render-check-label seed reportable-results [tooltip? #t])
    ; xml
    (let (; (U 'check-success 'check-warning 'check-failure 'check-exn)
          [class (check-results->class reportable-results)])
      (xml (span (@ [class ,(string-append "check-label" (if tooltip? " tooltip-anchor" ""))])
                 ,(opt-xml (not (eq? class 'check-success))
                    ,(check-result-icon class)
                    ,(render-check-results seed reportable-results tooltip?))))))
  
  ; seed (listof check-result) [boolean] -> xml
  (define/private (render-check-results seed results [tooltip? #t])
    (xml (ul (@ [class ,(string-append "check-results" (if tooltip? " tooltip" ""))])
             ,@(for/list ([result results])
                 (xml (li (@ [class ,(check-result->class result)])
                          ,(check-result-message result)))))))
  
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
  
  ; -> boolean
  (define/public (value-valid?)
    (not (get-value-error)))
  
  ; -> (U string #f)
  (define/public (get-value-error)
    (ormap (cut send <> get-value-error)
           (get-form-elements)))
  
  ; boolean -> void
  (define/public (set-enabled?! enabled?)
    (for-each (cut send <> set-enabled?! enabled?) (get-form-elements)))
  
  ; -> boolean
  (define/public (get-enabled?)
    (ormap (cut send <> get-enabled?) (get-form-elements)))
  
  ; -> (listof check-result)
  (define/public (parse)
    (for/append ([form-element (in-list (get-form-elements))])
      (let ([err (send form-element get-value-error)])
        (if err
            (check/annotate ([ann:form-elements (list form-element)])
              (check-fail err))
            null))))
  
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

; (listof attribute) (U form-element<%> #f) -> (check-result -> boolean)
(define (make-check-result-filter/attributes+editor attributes editor)
  (cond [(and (and attributes (pair? attributes)) editor)
         (lambda (result)
           (or (memq editor (check-result-annotation result ann:form-elements))
               (ormap (cut check-result-has-attribute? result <>) attributes)))]
        [(and attributes (pair? attributes))
         (lambda (result)
           (ormap (cut check-result-has-attribute? result <>) attributes))]
        [editor
         (lambda (result)
           (memq editor (check-result-annotation result ann:form-elements)))]
        [else (error "filter-results: Either attribute or editor must be specified.")]))

; (listof check-result) (listof attribute) (U form-element% #f) -> (listof check-result)
(define (filter-results results attributes editor)
  (filter (make-check-result-filter/attributes+editor attributes editor) results))

; (listof check-result) attribute -> (listof check-result)
(define (filter-results/attribute results attribute)
  (filter-results results (list attribute) #f))

; (listof check-result) (listof attribute) -> (listof check-result)
(define (filter-results/attributes results attributes)
  (filter-results results attributes #f))

; (listof check-result) form-element% -> (listof check-result)
(define (filter-results/editor results editor)
  (filter-results results null editor))

; Provide statements -----------------------------

(provide entity-editor<%>
         entity-editor-mixin
         entity-editor%)

(provide/contract 
 [filter-results            (-> (listof check-result?) 
                                (listof attribute?)
                                (or/c (is-a?/c form-element<%>) #f)
                                (listof check-result?))]
 [filter-results/attribute  (-> (listof check-result?) 
                                attribute?
                                (listof check-result?))]
 
 [filter-results/attributes (-> (listof check-result?) 
                                (listof attribute?)
                                (listof check-result?))]
 [filter-results/editor     (-> (listof check-result?) 
                                (is-a?/c form-element<%>)
                                (listof check-result?))])