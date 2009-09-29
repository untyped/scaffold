#lang scheme/base

(require "base.ss")

(require (planet untyped/dispatch:3)
         (planet untyped/dispatch:3/core)
         (planet untyped/unlib:3/symbol)
         (planet untyped/snooze:3))

(define (entity-arg entity)
  (let ([name    (symbol-append (entity-name entity) '-arg)]
        [struct? (entity-cached-predicate entity)])
    (make-arg
     "[0-9]+"
     (lambda (raw)
       (let* ([num  (string->number raw)]
              [guid (and num (entity-make-vanilla-guid entity num))])
         (and guid (let-sql ([e entity])
                            (select-one #:from e #:where (= e.guid ,guid))))))
     (lambda (val)
       (cond [(struct? val) (format "~a" (snooze-struct-id val))]
             [(string? val) val]
             [else          (raise-type-error
                             name
                             (format "(U ~a string)" (entity-name entity))
                             val)])))))

; Provides ---------------------------------------

(provide/contract
 [entity-arg (-> entity? arg?)])
