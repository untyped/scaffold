#lang scheme/base

(require "base.ss")

(require "util.ss")

; Interfaces -------------------------------------

(define editor<%>
  (interface ()
    value-changed? ; -> boolean
    parse          ; -> (listof check-result)
    validate))     ; -> (listof check-result)

(define compound-editor<%>
  (interface (form-element<%>)
    get-sub-editors))  ; -> (listof form-element<%>)

; Mixins -----------------------------------------

(define-mixin compound-editor-mixin () (compound-editor<%>)
  
  ; Fields ---------------------------------------
  
  ; (cellof (listof form-element<%>))
  (init-cell sub-editors #:accessor #:mutator)
  
  ; Methods --------------------------------------
  
  ; -> boolean
  (define/public (value-changed?)
    (ormap (cut send <> value-changed?) (get-sub-editors)))
  
  ; -> boolean
  (define/public (value-valid?)
    (andmap (cut send <> value-valid?) (get-sub-editors)))
  
  ; -> (U string #f)
  (define/public (get-value-error)
    (ormap (cut send <> get-value-error) (get-sub-editors)))
  
  ; boolean -> void
  (define/public (set-enabled?! enabled?)
    (for-each (cut send <> set-enabled?! enabled?) (get-sub-editors)))
  
  ; -> boolean
  (define/public (get-enabled?)
    (ormap (cut send <> get-enabled?) (get-sub-editors)))
  
  ; -> any
  (define/public (get-value)
    (error "compound-editor.get-value must be overridden."))
  
  ; any -> void
  (define/public (set-value! val)
    (error "compound-editor.set-value! must be overridden.")))

; Classes ----------------------------------------

(define compound-editor%
  (compound-editor-mixin html-element%))

; Provide statements -----------------------------

(provide editor<%>
         compound-editor<%>
         compound-editor-mixin
         compound-editor%)
