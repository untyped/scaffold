#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "entity-view.ss"
         "page-internal.ss")

; Interfaces -------------------------------------

(define entity-delete-page<%>
  (interface ()
    get-entity
    get-value
    set-value!))

; Mixins -----------------------------------------

(define-mixin entity-delete-page-mixin (html-element<%> html-page<%>) (entity-delete-page<%>)
  
  (inherit get-id)
  
  ; Fields ----------------------------
  
  ; submit-button%
  (field submit-button
    (new submit-button%
         [action (callback on-delete)]
         [label  "Delete"])
    #:child)
  
  ; entity
  (init entity)
  
  ; (listof symbol)
  (init [classes null])
  
  (super-new [classes (cons 'scaffolded-delete-page classes)])
  
  ; (listof attribute)
  (init [auto-attributes (and entity (entity-data-attributes entity))])
  
  ; entity-view%
  (init-field view
    (new entity-view%
         [id              (symbol-append (get-id) '-view)]
         [entity          entity]
         [auto-attributes auto-attributes])
    #:child)
  
  ; Methods ---------------------------
  
  ; -> entity
  (define/public (get-entity)
    (send view get-entity))
  
  ; -> (U snooze-struct #f)
  (define/public (get-value)
    (send view get-value))
  
  ; (U snooze-struct #f) -> void
  (define/public (set-value! struct)
    (send view set-value! struct))
  
  ; -> string
  (define/override (get-title)
    (let* ([title  (super get-title)]
           [entity (get-entity)]
           [struct (get-value)])
      (cond #;[title title]
            [struct (format "Delete ~a: ~a" (entity-pretty-name entity) (format-snooze-struct struct))]
            [else   (format "Delete ~a" (entity-pretty-name entity))])))
  
  ; seed -> xml
  (define/augment (render seed)
    (let* ([value   (get-value)]
           [results (check-old-snooze-struct value)])
      (cond [(check-fatals?   results) (render/fatals   seed value results)]
            [(check-failures? results) (render/failures seed value results)]
            [(check-warnings? results) (render/warnings seed value results)]
            [else                      (render/success  seed value results)])))
  
  ; seed snooze-struct (listof check-result) -> xml
  (define/public (render/fatals seed value results)
    (raise (or (for/or ([result (in-list results)])
                 (and (check-fatal? result)
                      (check-result-exn result)))
               (error "no exception found"))))
  
  ; seed snooze-struct (listof check-result) -> xml
  (define/public (render/failures seed value results)
    (xml (p "This " ,(entity-name (get-entity)) " cannot be deleted for the following reasons:")
         (ul ,@(for/filter ([result (in-list results)])
                 (and (check-failure? result)
                      (xml (li ,(check-result-message result))))))))
  
  ; seed snooze-struct (listof check-result) -> xml
  (define/public (render/warnings seed value results)
    (xml (p "You are about to delete the " ,(entity-name (get-entity)) " below:")
         ,(send view render seed)
         (p "If you continue, be aware of the following:")
         (ul ,@(for/filter ([result (in-list results)])
                 (and (check-warning? result)
                      (xml (li ,(check-result-message result))))))
         (p "Only click \"" ,(send submit-button get-label) "\" if you are sure you wish to continue. " (strong "There is no undo! "))
         (p ,(send submit-button render seed))))
  
  ; seed snooze-struct (listof check-result) -> xml
  (define/public (render/success seed value results)
    (xml (p "You are about to delete the " ,(entity-name (get-entity)) " below:")
         ,(send view render seed)
         (p "Only click \"" ,(send submit-button get-label) "\" if you are sure you wish to continue. " (strong "There is no undo! "))
         (p ,(send submit-button render seed))))
  
  ; snooze-struct -> xml
  (define/public (get-delete-notification struct)
    (xml "Successfully deleted " ,(entity-pretty-name (snooze-struct-entity struct)) ": "
         ,(format-snooze-struct struct) "."))
  
  ; -> snooze-struct
  (define/public #:callback/return (on-delete)
    (let ([struct (get-value)])
      (call-with-transaction
       #:metadata (list (format "Delete ~a" (format-snooze-struct struct)))
       (lambda ()
         (begin0
           (delete! struct)
           (clear-continuation-table!)
           (notifications-add! (get-delete-notification struct))))))))

; Procedures -------------------------------------

; entity [(subclassof html-page%)] -> html-page%
(define (scaffold-delete-page entity [page% (default-scaffolded-page-superclass)])
  (new (entity-delete-page-mixin page%) [entity entity]))

; Provide statements -----------------------------

(provide entity-delete-page<%>
         entity-delete-page-mixin)

(provide/contract
 [scaffold-delete-page (->* (entity?) ((subclass?/c html-page%)) (is-a?/c html-page%))])
