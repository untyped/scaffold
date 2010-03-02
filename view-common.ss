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

; Helpers ----------------------------------------

; snooze-struct -> xml+quotable
(define (format-snooze-struct/link val)
  (cond [(review-controller-set? val)
         (xml (a (@ [href ,(review-controller-url val)])
                 ,(format-snooze-struct val)))]
        [else (format-snooze-struct val)]))

; seed (listof snooze-struct) -> xml
(define (render-related-structs seed relateds)
  (xml (ul (@ [class "relationship-view"])
           ,@(for/list ([related (in-list relateds)])
               (xml (li ,(format-snooze-struct/link related)))))))

; Provides ---------------------------------------

(provide/contract 
 [attribute-xml-defaults         (parameter/c (-> attribute? any/c xml+quotable?))]
 [attribute-xml                  (-> attribute? any/c xml+quotable?)]
 [snooze-struct-xml-ref-defaults (parameter/c (-> snooze-struct? attribute? xml+quotable?))]
 [snooze-struct-xml-ref          (-> snooze-struct? attribute? xml+quotable?)]
 [snooze-struct-csv-ref-defaults (parameter/c (-> snooze-struct? attribute? string?))]
 [snooze-struct-csv-ref          (-> snooze-struct? attribute? string?)]
 [render-related-structs         (-> seed? (listof snooze-struct?) xml?)])