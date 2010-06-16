#lang scheme/base

(require "../../test-base.ss")

(require srfi/13
         (unlib-in string symbol)
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

; Tests ------------------------------------------

(define/provide-test-suite review-tests
  
  #:before
  (lambda ()
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
  
  #:after
  (lambda ()
    (recreate-tables))
  
  (test-case "page title"
    (open/wait (controller-url sink-review test-sink))
    (check-equal? (title-ref) (format-snooze-struct test-sink) "default title")
    (open/wait (controller-url sink-review/vanilla test-sink))
    (check-equal? (title-ref) "A vanilla entity-view" "custom title"))
  
  (test-case "uncustomized page displays all attributes"
    (open/wait (controller-url sink-review test-sink))
    (for ([i    (in-naturals)]
          [attr (in-list (entity-data-attributes kitchen-sink))])
      (check-attr/defaults test-sink attr i))
    (check-equal? (node-count (node/jquery "table tr"))
                  (length (entity-data-attributes kitchen-sink))))
  
  (test-case "sink-review-page/vanilla"
    (open/wait (controller-url sink-review/vanilla test-sink))
    (for ([i    (in-naturals)]
          [attr (in-list (entity-data-attributes kitchen-sink))])
      (check-attr/defaults test-sink attr i))
    (check-equal? (node-count (node/jquery "table tr"))
                  (length (entity-data-attributes kitchen-sink))))
  
  (test-case "sink-review-page/attrs"
    (open/wait (controller-url sink-review/attrs test-sink))
    (for ([i    (in-naturals)]
          [attr (in-list (attr-list kitchen-sink a-boolean a-real a-integer))])
      (check-attr/defaults test-sink attr i))
    (check-equal? (node-count (node/jquery "table tr")) 3))
  
  (test-case "sink-review-page/customized-attrs"
    (open/wait (controller-url sink-review/customized-attrs test-sink))
    (check-attr test-sink (attr kitchen-sink a-boolean) 0 "A boolean" "yes")
    (check-attr test-sink (attr kitchen-sink a-real) 1 "A real, customized" "<b>4.56</b>")
    (check-attr test-sink (attr kitchen-sink a-integer) 2 "A integer" "123")
    (check-equal? (node-count (node/jquery "table tr")) 3))
  
  (test-case "sink-review-page/compound-attrs"
    (open/wait (controller-url sink-review/compound-attrs test-sink))
    (check-attr/defaults test-sink (attr kitchen-sink a-boolean) 0)
    (check-attr/defaults test-sink (attr kitchen-sink a-real) 1)
    (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/tag "table")))
                  "A-integer+a-string")
    (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/tag "table")))
                  (xml->string (xml (span "123 >> abc"))))
    (check-attr/defaults test-sink (attr kitchen-sink a-symbol) 3)
    (check-equal? (node-count (node/jquery "table tr")) 4))
  
  (test-case "sink-review-page/related-attrs"
    (open/wait (controller-url sink-review/related-attrs test-sink))
    (check-attr/defaults test-sink (attr kitchen-sink a-boolean) 0)
    (check-attr/defaults test-sink (attr kitchen-sink a-real) 1)
    (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/tag "table")))
                  "All posts")
    (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/tag "table")))
                  (xml->string (xml (ul (@ [class "relationship-view"])
                                        (li ,(snooze-struct-xml-ref test-sink (attr kitchen-sink a-post)))
                                        (li ,(snooze-struct-xml-ref test-sink (attr kitchen-sink a-required-post)))))))
    (check-attr/defaults test-sink (attr kitchen-sink a-integer) 3)
    (check-attr/defaults test-sink (attr kitchen-sink a-string) 4)
    (check-attr/defaults test-sink (attr kitchen-sink a-symbol) 5)
    (check-equal? (node-count (node/jquery "table tr")) 6)))
