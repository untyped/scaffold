#lang scheme/base

(require "base.ss")

(require "controller-internal.ss")

; Procedures -------------------------------------

; (parameter (attribute any -> xml))
(define attribute-xml-defaults
  (make-parameter
   (lambda (attr val)
     val)))

; attribute any -> xml
(define (attribute-xml attr val)
  ((attribute-xml-defaults) attr val))

; (parameter (snooze-struct attribute -> xml))
(define snooze-struct-xml-ref-defaults
  (make-parameter
   (lambda (struct attr)
     (let ([type (attribute-type attr)]
           [val  (snooze-struct-ref struct attr)])
       (cond [(snooze-struct? val)
              (format-snooze-struct/link val)]
             [(guid? val)
              (format-snooze-struct/link (find-by-guid val))]
             [(and (enum-type? type)
                   (let ([enum (enum-type-enum type)])
                     (and (enum-value? enum val) enum)))
              => (cut enum-prettify <> val)]
             [else val])))))

; snooze-struct attribute -> xml
(define (snooze-struct-xml-ref struct attr)
  ((snooze-struct-xml-ref-defaults) struct attr))

; Helpers ----------------------------------------

; snooze-struct -> xml+quotable
(define (format-snooze-struct/link val)
  (cond [(review-controller-set? val)
         (xml (a (@ [href ,(review-controller-url val)])
                 ,(format-snooze-struct val)))]
        [else (format-snooze-struct val)]))

; Provides ---------------------------------------

(provide/contract 
 [attribute-xml-defaults         (parameter/c (-> attribute? any/c xml+quotable?))]
 [attribute-xml                  (-> attribute? any/c xml+quotable?)]
 [snooze-struct-xml-ref-defaults (parameter/c (-> snooze-struct? attribute? xml+quotable?))]
 [snooze-struct-xml-ref          (-> snooze-struct? attribute? xml+quotable?)])