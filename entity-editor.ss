#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "attribute-editor.ss"
         "check-label.ss"
         "editor-internal.ss"
         "view-common.ss")

; Interfaces -------------------------------------

(define entity-editor<%>
  (interface (editor<%>)
    get-entity
    get-initial-value))

; Mixins -----------------------------------------

(define-mixin entity-editor-mixin (html-element<%>) (entity-editor<%>)
  
  (inherit core-html-attributes get-child-components)
  
  ; Fields -------------------------------------
  
  ; entity
  (init-field entity #:accessor)
  
  ; (listof attribute)
  (init-field attributes (and entity (entity-data-attributes entity)) #:accessor)
  
  ; (listof editor<%>)
  (init-field attribute-editors
    (or (and attributes (map default-attribute-editor attributes))
        (error "entity-editor constructor: insufficient arguments"))
    #:accessor #:children)
  
  ; (U snooze-struct #f)
  (cell initial-value #f #:accessor)
  
  (init [classes null])
  
  (super-new [classes (list* 'smoke-entity-editor 'ui-widget classes)])
  
  ; Methods ------------------------------------
  
  ; -> (listof editor<%>)
  (define/public (get-editors)
    (filter (cut is-a? <> editor<%>)
            (get-child-components)))
  
  ; seed -> xml
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper seed (render-editors seed (get-attribute-editors)))))
  
  ; seed xml+quotable -> xml
  (define/public (render-wrapper seed contents)
    (xml (table (@ ,(core-html-attributes seed))
                (tbody ,contents))))
  
  ; seed (listof editor<%>) -> xml
  (define/public (render-editors seed editors)
    (xml ,@(for/list ([editor (in-list editors)])
             (render-editor seed editor))))
  
  ; seed (listof attribute) -> xml
  (define/public (render-attributes seed attrs)
    (xml ,@(for/list ([attribute (in-list attrs)])
             (render-attribute-label+editor seed attribute))))
  
  ; seed editor -> xml
  (define/public (render-editor seed editor)
    (render-label+editor seed (send editor render-label seed) (send editor render seed)))
  
  ; seed attribute -> xml+quotable
  (define/public (render-attribute-label seed attribute)
    (attribute-label-xml attribute))
  
  ; seed attribute [editor%] -> xml
  (define/public (render-attribute-editor seed attribute [editor (get-attribute-editor attribute)])
    (opt-xml editor ,(send editor render seed)))
  
  ; seed attribute [editor%] -> xml
  (define/public (render-attribute-label+editor seed attribute [editor (get-attribute-editor attribute)])
    (render-label+editor seed 
                         (render-attribute-label  seed attribute)
                         (opt-xml editor ,(render-attribute-editor seed attribute editor))))
  
  ; attribute -> (U editor% #f)
  (define/private (get-attribute-editor attribute)
    (for/or ([ed (in-list (get-attribute-editors))])
      (and (equal? (list attribute) (send ed get-attributes)) ed)))
  
  ; seed xml+quotable xml -> xml+quotable
  (define/public (render-label+editor seed label-xml editor-xml)
    (xml (tr (th (@ [class "attribute-label"]) ,label-xml)
             (td (@ [class "attribute-value"]) ,editor-xml))))
  
  ; (listof check-result) -> void
  (define/public (set-check-results! results)
    (for-each (cut send <> set-check-results! results)
              (get-editors)))
  
  ; -> snooze-struct
  (define/public (get-value)
    (let ([init (get-initial-value)])
      (if (snooze-struct? init)
          (for/fold ([val init])
                    ([editor (in-list (get-attribute-editors))])
                    (send editor restructure val))
          (raise-type-error 'entity-editor.get-value "snooze-struct" #f))))
  
  ; snooze-struct -> void
  (define/public (set-value! val)
    (unless (snooze-struct? val)
      (raise-type-error 'entity-editor.set-value! "snooze-struct" val))
    (web-cell-set! initial-value-cell val)
    (for ([editor (in-list (get-attribute-editors))])
      (send editor destructure! val)))
  
  ; -> boolean
  (define/public (value-changed?)
    (ormap (cut send <> value-changed?)
           (get-editors)))
  
  ; -> (listof check-result)
  (define/public (parse)
    (apply check-problems (map (cut send <> parse) (get-editors))))
  
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

; Provide statements -----------------------------

(provide entity-editor<%>
         entity-editor-mixin
         entity-editor%)
