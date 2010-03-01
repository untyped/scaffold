#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "attribute-editor-internal.ss"
         "check-label.ss"
         "editor-internal.ss")

; Classes ----------------------------------------

(define-class foreign-key-editor% (complete-attribute-editor-mixin vanilla-combo-box%) ()
  
  ; Constructor --------------------------------
  
  ; (listof attribute)
  (init [attributes null])
  
  (super-new [attributes attributes])
  
  ; Fields -------------------------------------
  
  ; (U entity #f)
  (init-field entity
    (and (pair? attributes)
         (guid-type? (attribute-type (car attributes)))
         (guid-type-entity (attribute-type (car attributes))))
    #:accessor)
  
  ; (cell (U sql-expr (-> sql-expr) #f))
  (init-cell where #f #:mutator)
  
  ; (cell (listof sql-order))
  (init-cell order
             (entity-default-order (get-entity))
             #:mutator)
  
  ; Methods ------------------------------------
  
  ; -> (U sql-expr #f)
  (define/public (get-where)
    (let ([val (web-cell-ref where-cell)])
      (if (procedure? val)
          (val)
          val)))
  
  ; -> (listof sql-order)
  (define/public (get-order)
    (let ([val (web-cell-ref order-cell)])
      (if (procedure? val)
          (val)
          val)))
  
  ; -> (listof (cons integer string))
  (define/override (get-options)
    (let-sql ([entity (get-entity)])
      (list* #f (select-all #:from  entity
                            #:where ,(get-where)
                            #:order ,(get-order)))))
  
  ; (U snooze-struct #f) -> (U string #f)
  (define/override (option->raw option)
    (and option
         (if (and (snooze-struct?       option)
                  (snooze-struct-saved? option))
             (number->string (snooze-struct-id option))
             (raise-type-error 'foreign-key-editor.option->raw
                               "(U snooze-struct #f)"
                               option))))
  
  ; (U string #f) -> snooze-struct
  (define/override (raw->option raw)
    (and raw (let ([id (string->number raw)])
               (and id (find-by-id entity id)))))
  
  ; (U snooze-struct #f) -> string
  (define/override (option->string option)
    (if option
        (format-snooze-struct option)
        (format "-- No ~a selected --" (entity-pretty-name entity)))))

; Provide statements -----------------------------

(provide foreign-key-editor%)
