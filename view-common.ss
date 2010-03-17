#lang scheme/base

(require "base.ss")

(require (only-in (unlib-in string) string-sentencecase)
         "controller-internal.ss")

; Procedures -------------------------------------

; (parameter (attribute any -> xml))
(define attribute-xml-defaults
  (make-parameter
   (lambda (attr val)
     val)))

; attribute any -> xml
(define (attribute-xml attr val)
  ((attribute-xml-defaults) attr val))


; (parameter (attribute -> xml))
(define attribute-label-xml-defaults
  (make-parameter
   (lambda (attr)
     (string-sentencecase (attribute-pretty-name attr)))))

; attribute -> xml
(define (attribute-label-xml attr)
  ((attribute-label-xml-defaults) attr))

; (parameter (snooze-struct attribute -> xml))
(define snooze-struct-xml-ref-defaults
  (make-parameter
   (lambda (struct attr)
     (let ([type (attribute-type attr)]
           [val  (snooze-struct-ref struct attr)])
       (cond [(snooze-struct? val)
              (snooze-struct-link val)]
             [(guid? val)
              (snooze-struct-link (find-by-guid val))]
             [(and (enum-type? type)
                   (let ([enum (enum-type-enum type)])
                     (and (enum-value? enum val) enum)))
              => (cut enum-prettify <> val)]
             [else val])))))

; snooze-struct attribute -> xml
(define (snooze-struct-xml-ref struct attr)
  ((snooze-struct-xml-ref-defaults) struct attr))

; (parameter (snooze-struct attribute -> csv-cell))
(define snooze-struct-csv-ref-defaults
  (make-parameter
   (lambda (struct attr)
     (let ([type (attribute-type attr)]
           [val  (snooze-struct-ref struct attr)])
       (cond [(snooze-struct? val)
              (format-snooze-struct val)]
             [(guid? val)
              (format-snooze-struct (find-by-guid val))]
             [(and (enum-type? type)
                   (let ([enum (enum-type-enum type)])
                     (and (enum-value? enum val) enum)))
              => (cut enum-prettify <> val)]
             [else val])))))

; snooze-struct attribute -> csv-cell
(define (snooze-struct-csv-ref struct attr)
  ((snooze-struct-csv-ref-defaults) struct attr))

; seed (listof snooze-struct) [snooze-struct -> xml] -> xml
(define (render-related-structs seed relateds [render-related snooze-struct-link])
  (xml (ul (@ [class "relationship-view"])
           ,@(for/list ([related (in-list relateds)])
               (xml (li ,(render-related related)))))))

; Helpers ----------------------------------------

; snooze-struct -> xml+quotable
(define (snooze-struct-link val)
  (cond [(and (review-controller-set? val) (controller-access? (review-controller-ref val) val))
         (xml (a (@ [href ,(review-controller-url val)])
                 ,(format-snooze-struct val)))]
        [else (format-snooze-struct val)]))

; Provides ---------------------------------------

(provide/contract 
 [attribute-xml-defaults         (parameter/c (-> attribute? any/c xml+quotable?))]
 [attribute-xml                  (-> attribute? any/c xml+quotable?)]
 [attribute-label-xml-defaults   (parameter/c (-> attribute? xml+quotable?))]
 [attribute-label-xml            (-> attribute? xml+quotable?)]
 [snooze-struct-xml-ref-defaults (parameter/c (-> snooze-struct? attribute? xml+quotable?))]
 [snooze-struct-xml-ref          (-> snooze-struct? attribute? xml+quotable?)]
 [snooze-struct-csv-ref-defaults (parameter/c (-> snooze-struct? attribute? string?))]
 [snooze-struct-csv-ref          (-> snooze-struct? attribute? string?)]
 [render-related-structs         (->* (seed? (listof snooze-struct?))
                                      ((-> snooze-struct? xml?))
                                      xml?)])