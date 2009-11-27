#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "attribute-editor-internal.ss"
         "check-label.ss"
         "editor-internal.ss")

; Classes ----------------------------------------

(define foreign-key-editor%
  (class/cells (complete-attribute-editor-mixin vanilla-combo-box%) ()
    
    ; Fields -------------------------------------
    
    ; (U entity #f)
    (init-field entity #:accessor)
    
    ; (cell (U sql-expr #f))
    (init-cell where #f #:accessor #:mutator)
    
    ; (cell (listof sql-order))
    (init-cell order
      (entity-default-order (get-entity))
      #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
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
          (format "-- No ~a selected --" (entity-pretty-name entity))))))

; Provide statements -----------------------------

(provide foreign-key-editor%)
