#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "entity-view.ss"
         "page-internal.ss")

; Interfaces -------------------------------------

(define entity-review-page<%>
  (interface ()
    get-entity
    get-value
    set-value!))

; Mixins -----------------------------------------

(define-mixin entity-review-page-mixin (html-element<%> html-page<%>) (entity-review-page<%>)
  
  (inherit get-id)
  
  ; Fields ----------------------------
  
  (super-new)
  
  ; entity
  (init [entity #f])
  
  ; (listof attribute)
  (init [attributes (and entity (entity-data-attributes entity))])
  
  ; entity-view%
  (init-field view
    (or (and entity
             attributes
             (new entity-view%
                  [id         (symbol-append (get-id) '-view)]
                  [entity     entity]
                  [attributes attributes]))
        (string-append "entity-review-page constructor: insufficient arguments"))
    #:child)
  
  ; Methods ---------------------------
  
  ; -> entity
  (define/public (get-entity)
    (send view get-entity))
  
  ; -> (U snooze-struct #f)
  (define/public (get-value)
    (send view get-value))
  
  ; snooze-struct -> void
  (define/public (set-value! struct)
    (send view set-value! struct))
  
  ; -> string
  (define/override (get-title)
    (let* ([title  (super get-title)]
           [entity (get-entity)]
           [struct (get-value)])
      (cond [title title]
            [struct (format-snooze-struct struct)]
            [else   (entity-pretty-name entity)])))
  
  ; seed -> xml
  (define/augment (render seed)
    (send view render seed)))

; Procedures -------------------------------------

; entity [(subclassof html-page%)] -> html-page%
(define (scaffold-review-page entity [page% (default-scaffolded-page-superclass)])
  (new (entity-review-page-mixin page%) [entity entity]))

; Provide statements -----------------------------

(provide entity-review-page<%>
         entity-review-page-mixin)

(provide/contract
 [scaffold-review-page (->* (entity?) ((subclass?/c html-page%)) (is-a?/c html-page%))])
