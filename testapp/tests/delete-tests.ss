#lang scheme/base

(require "../../test-base.ss")

(require srfi/13
         srfi/19
         (unlib-in string symbol time)
         "../content-base.ss")

; Helpers ----------------------------------------

(define test-user #f)
(define test-post1 #f)
(define test-post2 #f)
(define test-sink #f)

; snooze-struct attribute natural [string] -> void
(define-check (check-attr/defaults struct attr row-num)
  (let* ([expected-key (xml+quotable->string (attribute-label-xml attr))]
         [expected-val (xml+quotable->string (snooze-struct-xml-ref struct attr))])
    (check-attr struct attr row-num expected-key expected-val)))

; snooze-struct attribute natural [string] -> void
(define-check (check-attr struct attr row-num expected-key expected-val)
  (let* ([actual-key (inner-html-ref (node/cell/xy 0 row-num (node/tag "table")))]
         [actual-val (inner-html-ref (node/cell/xy 1 row-num (node/tag "table")))])
    (with-check-info (['actual-key actual-key]
                      ['actual-val actual-val]
                      ['expected-key expected-key]
                      ['expected-val expected-val])
      (check-equal? actual-key expected-key)
      (check-equal? actual-val expected-val))))

; -> void
(define (recreate-test-data!)
  (recreate-tables)
  (set! test-user  (save! (make-user/defaults
                           #:username "username"
                           #:surname "Surname"
                           #:forenames "Forenames"
                           #:email "email@example.com")))
  (set! test-post1 (save! (make-post/defaults
                           #:user test-user
                           #:subject "Subject1")))
  (set! test-post2 (save! (make-post/defaults
                           #:user test-user
                           #:subject "Subject2")))
  (set! test-sink  (save! (make-kitchen-sink/defaults
                           #:a-boolean #t
                           #:a-integer 123
                           #:a-real  4.56
                           #:a-string "abc"
                           #:a-symbol 'def
                           #:a-10-char-string "abcdefghij"
                           #:a-10-char-symbol 'defghijklm
                           #:a-time-utc (st-utc "2010-01-01 12:00")
                           #:a-time-tai (st-tai "2009-01-01 12:00")
                           #:a-short-enum (short-enum a)
                           #:a-long-enum (long-enum b)
                           #:a-post test-post1
                           #:a-required-integer 12345
                           #:a-required-real 4.5678
                           #:a-required-string "abcde"
                           #:a-required-symbol 'defgh
                           #:a-required-10-char-string "ghijklmnop"
                           #:a-required-10-char-symbol 'jklmnopqrs
                           #:a-required-time-utc (st-utc "2008-01-01 12:00")
                           #:a-required-time-tai (st-tai "2007-01-01 12:00")
                           #:a-required-short-enum (short-enum c)
                           #:a-required-long-enum (long-enum d)
                           #:a-required-post test-post2))))

; Tests ------------------------------------------

(define/provide-test-suite delete-tests
  
  #:after
  (lambda ()
    (recreate-tables))
  
  (test-case "page title and structure"
    (recreate-test-data!)
    (open/wait (controller-url sink-delete test-sink))
    (check-equal? (title-ref) (format "Delete kitchen sink: ~a" (format-snooze-struct test-sink)))
    (for ([i    (in-naturals)]
          [attr (in-list (entity-data-attributes kitchen-sink))])
      (check-attr/defaults test-sink attr i))
    (check-equal? (node-count (node/jquery "table tr"))
                  (length (entity-data-attributes kitchen-sink)))
    (check-pred node-exists? (node/jquery "input:submit[value=Delete]")))

  (test-case "delete button deletes the struct"
    (recreate-test-data!)
    (open/wait (controller-url sink-delete test-sink))
    (check-equal? (find-kitchen-sinks) (list test-sink))
    (click/wait (node/jquery "input:submit[value=Delete]"))
    (check-equal? (title-ref) "Kitchen sinks")
    (check-true (node-exists? (node/jquery "div.notification:contains(Successfully deleted)")))
    (check-equal? (find-kitchen-sinks) null)))