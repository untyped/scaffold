#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "attribute-editor.ss"
         "editor-controller.ss"
         "editor-internal.ss"
         "entity-editor.ss"
         "page-internal.ss"
         "util.ss")

; Mixins -----------------------------------------

(define-mixin editor-page-inner-mixin (html-element<%> html-page<%> editor-controller<%>) ()
  
  (inherit get-id
           get-editor
           on-update)
  
  ; Fields ----------------------------
  
  ; editor%
  (init [editor (error "editor-page constructor: insufficient arguments")])
  
  (super-new [editor editor])
  
  (send editor set-id! (symbol-append (get-id) '-editor))
  
  ; submit-button%
  (field submit-button
    (new submit-button%
         [id     (symbol-append (get-id) '-submit)]
         [action (callback on-update)])
    #:child #:accessor)
  
  ; Methods ---------------------------
  
  ; -> (listof html-component<%>)
  (define/override (get-child-components)
    (cons (get-editor)
          (super get-child-components)))
  
  ; -> (listof (U xml (seed -> xml)))
  (define/augment (get-html-requirements)
    (list* snooze-styles
           (inner null get-html-requirements)))
  
  ; -> (U snooze-struct #f)
  (define/public (get-value)
    (send (get-editor) get-value))
  
  ; snooze-struct -> void
  (define/public (set-value! struct)
    (send (get-editor) set-value! struct))
  
  ; seed -> xml
  (define/augment (render seed)
    (xml ,(send (get-editor) render seed)
         ,(send submit-button render seed))))

(define editor-page-mixin
  (compose editor-page-inner-mixin editor-controller-mixin))


(define-mixin entity-editor-page-inner-mixin (html-element<%> html-page<%> editor-controller<%>) ()
  
  (inherit get-id
           get-editor
           on-update)
  
  ; Fields ----------------------------
  
  ; entity
  (init [entity #f])
  
  ; (listof attribute)
  (init [attributes (and entity (entity-data-attributes entity))])
  
  ; (listof attribute-editor<%>)
  (init [attribute-editors (and attributes (map default-attribute-editor attributes))])
  
  ; entity-editor%
  (init [editor (or (and entity
                         attribute-editors
                         (new entity-editor%
                              [entity            entity]
                              [attribute-editors attribute-editors]))
                    (error "entity-editor-page constructor: insufficient arguments"))])
  
  (super-new [editor editor])
  
  ; Methods ---------------------------
  
  ; -> entity
  (define/public (get-entity)
    (send (get-editor) get-entity))
  
  ; -> (U snooze-struct #f)
  (define/public (get-initial-value)
    (send (get-editor) get-initial-value))
  
  ; -> string
  (define/override (get-title)
    (let* ([title  (super get-title)]
           [entity (get-entity)]
           [struct (get-initial-value)])
      (cond [title title]
            [(and struct (snooze-struct-saved? struct))
             (format "Edit ~a: ~a" (entity-pretty-name entity) (format-snooze-struct struct))]
            [struct (format "New ~a" (entity-pretty-name entity))]
            [else   (format "Editing ~a" (entity-pretty-name entity))]))))


(define entity-editor-page-mixin
  (compose entity-editor-page-inner-mixin editor-page-mixin))

; Procedures -------------------------------------

; entity [(subclassof html-page%)] -> html-page%
(define (scaffold-create-page entity [page% (default-scaffolded-page-superclass)])
  (new (entity-editor-page-mixin page%) [entity entity]))

; entity [(subclassof html-page%)] -> html-page%
(define (scaffold-update-page entity [page% (default-scaffolded-page-superclass)])
  (new (entity-editor-page-mixin page%) [entity entity]))

; Provide statements -----------------------------

(provide entity-editor-page-mixin
         editor-page-mixin)

(provide/contract
 [scaffold-create-page (->* (entity?) ((subclass?/c html-page%)) (is-a?/c html-page%))]
 [scaffold-update-page (->* (entity?) ((subclass?/c html-page%)) (is-a?/c html-page%))])
