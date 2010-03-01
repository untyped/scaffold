#lang scheme/base

(require "base.ss")

(require "checkable.ss"
         "util.ss")

; Interfaces -------------------------------------

(define editor<%>
  (interface (checkable<%>)
    get-editors          ; -> (listof editor<%>)
    value-changed?       ; -> boolean
    parse                ; -> (listof check-result)
    validate))           ; -> (listof check-result)

; Mixins -----------------------------------------

(define-mixin simple-editor-mixin/html-element (html-element<%>) (editor<%>)
  
  ; Fields ------------------------------
  
  (super-new)
  
  ; (listof editor<%>)
  (init-field editors null #:accessor #:children)
  
  ; Methods -----------------------------
  
  ; (listof check-result) -> void
  (define/public (set-check-results! results)
    (for-each (cut send <> set-check-results! results)
              (get-editors)))
  
  ; -> (listof check-result)
  (define/public (parse)
    (apply check-problems (map (cut send <> parse) (get-editors))))
  
  ; -> (listof check-result)
  (define/public (validate)
    (apply check-problems
           (map (cut send <> validate)
                (get-editors))))
  
  ; -> boolean
  (define/public (value-changed?)
    (or (ormap (cut send <> value-changed?)
               (get-editors)))))

(define-mixin simple-editor-mixin/form-element (form-element<%>) (editor<%>)
  
  (inherit get-value)
  
  ; Fields ------------------------------
  
  (super-new)
  
  ; (listof editor<%>)
  (init-field editors null #:accessor #:children)
  
  ; Methods -----------------------------
  
  ; (listof check-result) -> void
  (define/public (set-check-results! results)
    (for-each (cut send <> set-check-results! results)
              (get-editors)))
  
  ; -> (listof check-result)
  (define/public (parse)
    (apply check-problems
           (check/annotate ([ann:form-elements (list this)])
             (with-handlers ([exn:smoke:form? (lambda (exn) (check-fail (exn-message exn)))])
               (get-value)
               (check-pass)))
           (map (cut send <> parse) (get-editors))))
  
  ; -> (listof check-result)
  (define/public (validate)
    (apply check-problems (map (cut send <> validate) (get-editors))))
  
  ; -> boolean
  (define/override (value-changed?)
    (or (super value-changed?)
        (ormap (cut send <> value-changed?)
               (get-editors)))))

; html-element<%> -> editor<%>
(define (simple-editor-mixin class)
  (if (implementation? class form-element<%>)
      (simple-editor-mixin/form-element class)
      (simple-editor-mixin/html-element class)))

; Classes ----------------------------------------

(define simple-editor%
  (simple-editor-mixin html-element%))

; Provide statements -----------------------------

(provide editor<%>
         simple-editor-mixin
         simple-editor%)
