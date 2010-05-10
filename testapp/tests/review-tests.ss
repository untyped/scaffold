#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss"
         "../content-base.ss")

; Helpers ----------------------------------------

(define test-user #f)
(define test-post1 #f)
(define test-post2 #f)
(define test-sink #f)

; Tests ------------------------------------------

(define/provide-test-suite review-tests
  
  #:before
  (lambda ()
    (for-each delete! (find-users))
    (for-each delete! (find-posts))
    (for-each delete! (find-kitchen-sinks))
    (set! test-user  (save! (make-user/defaults
                             #:username "username"
                             #:surname "Surname"
                             #:forenames "Forenames"
                             #:email "email@example.com")))
    (set! test-post1 (save! (make-post/defaults
                             #:user user
                             #:subject "Subject1")))
    (set! test-post2 (save! (make-post/defaults
                             #:user user
                             #:subject "Subject2")))
    (set! test-sink  (save! (make-kitchen-sink/defaults
                             #:a-boolean #t
                             #:a-integer 123
                             #:a-real  4.56
                             #:a-string "abc"
                             #:a-symbol 'def
                             #:a-10-char-string "abcdefghij"
                             #:a-10-shar-symbol 'defghijklm
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
  
  #:after
  (lambda ()
    (for-each delete! (find-users))
    (for-each delete! (find-posts))
    (for-each delete! (find-kitchen-sinks)))
  
  (test-case "test-page displays"
    (open/wait (controller-url sink-review test-sink))
    (check-equal? (title-ref) "Editor")))
