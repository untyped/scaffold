#lang scheme/base

(require "base.ss")

(require (unlib-in string symbol)
         "check-label.ss"
         "controller-internal.ss"
         "view-internal.ss")

; Interfaces -------------------------------------

(define attribute-view<%>
  (interface (view<%> labelled-element<%>)
    get-attributes ; -> (listof attribute)
    destructure!)) ; snooze-struct -> void

; Mixins -----------------------------------------

(define attribute-view-mixin
  (mixin/cells (html-element<%> labelled-element<%> view<%>) (attribute-view<%>)
    
    (inherit core-html-attributes
             get-component-id
             get-id
             set-id!
             get-value
             set-value!
             render-label
             set-label!)
    
    ; Fields -------------------------------------
    
    (super-new)
    
    ; (cell (listof attribute))
    (init-cell attributes null #:accessor)
        
    (init [id    (if (pair? attributes)
                     (let ([attr (car attributes)])
                       (symbol-append (entity-name (attribute-entity attr)) '- (attribute-name attr)))
                     (get-component-id))]
          [label (if (pair? attributes)
                     (let ([attr (car attributes)])
                       (xml-quote (string-sentencecase (attribute-pretty-name attr))))
                     (xml-quote id))])
    
    (set-id!   id)
    (set-label! label)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (span (@ ,(core-html-attributes seed))
                 ,(let ([type       (attribute-type (car (get-attributes)))]
                        [val        (get-value)])
                    (cond [(snooze-struct? val)
                           (if (review-controller-set? val)
                               (xml (a (@ [href ,(review-controller-url val)])
                                       ,(format-snooze-struct val)))
                               (format-snooze-struct val))]
                          [(and (enum-type? type) (enum-type-enum type))
                           => (lambda (enum)
                                (enum-prettify enum val))]
                          [else val])))))
    
    ; snooze-struct -> snooze-struct
    (define/public (destructure! struct)
      (match (get-attributes)
        [(list-rest (? attribute? attr) _)
         (set-value! (snooze-struct-ref struct attr))]
        [attrs (raise-type-error 'attribute-view.destructure! "(list attribute attribute ...)" attrs)]))))

; Classes ----------------------------------------

(define complete-attribute-view-mixin
  (compose attribute-view-mixin check-label-mixin labelled-element-mixin simple-view-mixin))

(define attribute-view%
  (complete-attribute-view-mixin html-element%))

; Procedures -------------------------------------

; (parameter (attribute -> attribute-view%))
(define attribute-view-defaults
  (make-parameter
   (lambda (attr)
     (new attribute-view% [attributes (list attr)]))))

; attribute -> attribute-view<%>
(define (default-attribute-view attr)
  ((attribute-view-defaults) attr))

; Provide statements -----------------------------

(provide attribute-view<%>
         attribute-view-mixin
         attribute-view%)

(provide/contract
 [attribute-view-defaults (parameter/c procedure?)]
 [default-attribute-view  (-> attribute? (is-a?/c attribute-view<%>))])
