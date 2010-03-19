#lang scheme/base

(require "base.ss")

(require (smoke-in)
         (snooze-in)
         (unlib-in string symbol)
         "check-label.ss"
         "editor-internal.ss")

; Configuration ----------------------------------

; (parameter xml+quotable)
(define default-required-label
  (make-parameter " (required)"))

; Interfaces -------------------------------------

; Mixins -----------------------------------------

(define-mixin labelled-editor-mixin (form-element<%> labelled-element<%> editor<%> check-label<%>) ()
  
  (inherit get-component-id
           get-id 
           set-id!
           get-value
           set-value!
           render-check-label
           set-label!
           value-changed?)
  
  ; Fields -------------------------------------
  
  ; boolean
  (init-field required? #f
    #:accessor)
  
  (init-field required-label 
    (default-required-label)
    #:accessor
    #:mutator)
  
  ; Constructor --------------------------------
  
  (super-new)
  
  ; Methods ------------------------------------
  
  ; -> symbol
  (define/public (get-wrapper-id)
    (symbol-append (get-id) '-wrapper))
  
  ; seed -> xml
  (define/override (render-label-content seed)
    (if required?
        (xml ,(super render-label-content seed)
             ,(opt-xml required-label ,required-label))
        (super render-label-content seed)))
  
  ; seed -> xml
  (define/override (render seed)
    (xml (span (@ [id ,(get-wrapper-id)])
               ,(super render seed) " "
               ,(render-check-label seed))))
  
  ; -> (listof check-result)
  (define/override (parse)
    (check/annotate ([ann:form-elements (list this)])
      (check-until-problems
       (cut super parse)
       (cut with-handlers ([exn:smoke:form? (lambda (exn) (check-fail (exn-message exn)))])
            (if (and required? (not (get-value)))
                (check-fail "Value is required.")
                (check-pass))))))
  
  ; -> (listof check-result)
  (define/override (validate)
    (check/annotate ([ann:form-elements (list this)])
      (check-until-problems
       (cut super validate)
       (cut with-handlers ([exn:smoke:form? (lambda (exn) (check-fail (exn-message exn)))])
            (if (and required? (not (get-value)))
                (check-fail "Value is required.")
                (check-pass))))))
  
  ; seed -> js
  (define/override (get-on-render seed)
    (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(get-wrapper-id)))
                                "replace"
                                ,(xml->string (render seed)))))))

(define complete-editor-mixin
  (compose labelled-editor-mixin
           check-label-mixin
           simple-editor-mixin
           labelled-element-mixin))

; Classes ----------------------------------------

(define-class labelled-editor% 
  (labelled-element-mixin (check-label-mixin (simple-editor-mixin form-element%))) ()
  
  (inherit core-html-attributes
           get-component-id
           render-check-label)
  
  ; Methods ------------------------------------
  
  ; seed -> xml
  (define/overment (render seed)
    (xml (span (@ ,(core-html-attributes seed))
               ,(inner (xml) render seed) " "
               ,(render-check-label seed)))))

; Provide statements -----------------------------

(provide complete-editor-mixin 
         labelled-editor%)

#;(provide/contract
   [default-required-label (parameter/c xml+quotable?)])
