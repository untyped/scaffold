#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss")

; Helpers ----------------------------------------

; -> person
(define (make-dave)
  (make-user/defaults #:username  "david"
                      #:surname   "brooks"
                      #:forenames "david james"
                      #:email     "djb@example.com"))

; person -> post
(define (make-first-post user)
  (make-post/defaults #:user    user
                      #:subject "my first post"))

; -> kitchen-sink
(define (make-sink post)  
  (make-kitchen-sink/defaults
   #:a-boolean                 #f
   #:a-integer                 1
   #:a-real                    1.1
   #:a-string                  "one"
   #:a-symbol                  'one
   #:a-10-char-string          "oneoneone1"
   #:a-10-char-symbol          'oneoneone1
   #:a-time-utc                (date->time-utc (make-date 1 1 1 1 1 1 2001 1))
   #:a-time-tai                (date->time-tai (make-date 1 1 1 1 1 1 2001 1))
   #:a-short-enum              (short-enum a)
   #:a-long-enum               (long-enum a)
   #:a-post                    #f
   #:a-required-integer        2
   #:a-required-real           2.2
   #:a-required-string         "two"
   #:a-required-symbol         'two
   #:a-required-10-char-string "twotwotwo2"
   #:a-required-10-char-symbol 'twotwotwo2
   #:a-required-time-utc       (date->time-utc (make-date 2 2 2 2 2 2 2002 2))
   #:a-required-time-tai       (date->time-tai (make-date 2 2 2 2 2 2 2002 2))
   #:a-required-short-enum     (short-enum b)
   #:a-required-long-enum      (long-enum b)
   #:a-required-post           post))

; Tests ------------------------------------------

(define/provide-test-suite update-tests
  #:before (cut recreate-database)
  #:after  (cut recreate-database)
  
  (test-suite "kitchen sink update page 1 (entity-data-attributes)"
    
    (define dave (save! (make-dave)))
    (define post (save! (make-first-post dave)))
    (define sink (save! (make-sink post)))
    
    (test-case "update page displays"
      (open/wait (format "/sinks/~a" (snooze-struct-id sink)))
      (check-equal? (title-ref) "Editor")
      (check-equal? (inner-html-ref (node/id 'total-commits)) "0")))
  
  #;(test-case "correct values allow form submission"
      (enter-text (node/id 'larger-field) "2")
      (enter-text (node/id 'smaller-field) "1")
      (for ([i (in-range 1 4)])
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-equal? (inner-html-ref (node/id 'total-commits)) (number->string i))))
  
  #;(test-case "failures prevent form submission"
      (enter-text (node/id 'larger-field) "1")
      (enter-text (node/id 'smaller-field) "2")
      (for ([i (in-range 1 4)])
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-failure")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-failure")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) "3")))
  
  #;(test-case "warnings require confirmation"
      (for ([i (in-range 3 6)])
        (enter-text (node/id 'larger-field) (number->string i))
        (enter-text (node/id 'smaller-field) (number->string i))
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-warning")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-warning")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) (number->string i))
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-warning")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-warning")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) (number->string (add1 i)))))
  
  #;(test-case "changing field values without removing warnings never allows submission"
      (for ([i (in-range 6 10)])
        (enter-text (node/id 'larger-field) (number->string i))
        (enter-text (node/id 'smaller-field) (number->string i))
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-warning")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-warning")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) "6"))))

