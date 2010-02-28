#lang scheme/base

(require "base.ss")

(require (planet untyped/smoke:1/lib/dispatch/core)
         (planet untyped/unlib:3/symbol)
         (planet untyped/snooze:3))

(define (entity-arg entity)
  (let ([name    (symbol-append (entity-name entity) '-arg)]
        [struct? (entity-predicate entity)])
    (make-arg
     "[0-9]+"
     (lambda (raw)
       (let* ([num  (string->number raw)]
              [guid (and num (entity-make-guid entity num))])
         (and guid (find-by-guid guid))))
     (lambda (val)
       (cond [(and (snooze-struct? val) (eq? (snooze-struct-entity val) entity))
              (if (snooze-struct-saved? val)
                  (number->string (snooze-struct-id val))
                  (error "cannot refer to unsaved struct in URL" val))]
             [(and (guid? val) (eq? (guid-entity val) entity))
              (if (database-guid? val)
                  (number->string (guid-id val))
                  (error "cannot refer to temporary guid in URL" val))]
             [(number? val) (number->string val)]
             [else (raise-type-error
                    name
                    (format "(U ~a ~a-guid number)"
                            (entity-name entity)
                            (entity-name entity))
                    val)])))))

; Provides ---------------------------------------

(provide/contract
 [entity-arg (-> entity? arg?)])
