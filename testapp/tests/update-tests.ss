#lang scheme/base

(require srfi/13
         srfi/19
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss"
         "../content-base.ss")

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

; person -> post
(define (make-second-post user)
  (make-post/defaults #:user    user
                      #:subject "my second post"))

; -> kitchen-sink
(define (make-sink post)  
  (make-kitchen-sink/defaults
   #:a-boolean                 #f
   #:a-integer                 1
   #:a-real                    1.1
   #:a-string                  "onestr"
   #:a-symbol                  'onesym
   #:a-10-char-string          "onestrone1"
   #:a-10-char-symbol          'onesymone1
   #:a-time-utc                (date->time-utc (make-date 1 1 1 1 1 1 2001 1))
   #:a-time-tai                (date->time-tai (make-date 11 11 11 11 11 11 2011 1))
   #:a-short-enum              (short-enum a)
   #:a-long-enum               (long-enum b)
   #:a-post                    #f
   #:a-required-integer        2
   #:a-required-real           2.2
   #:a-required-string         "twostr"
   #:a-required-symbol         'twosym
   #:a-required-10-char-string "twostrtwo2"
   #:a-required-10-char-symbol 'twosymtwo2
   #:a-required-time-utc       (date->time-utc (make-date 2 2 2 2 2 2 2002 2))
   #:a-required-time-tai       (date->time-tai (make-date 22 22 22 22 22 2 2022 2))
   #:a-required-short-enum     (short-enum c)
   #:a-required-long-enum      (long-enum d)
   #:a-required-post           post))

; -> kitchen-sink
(define (make-sink-2 post)  
  (make-kitchen-sink/defaults
   #:a-boolean                 #t
   #:a-integer                 3
   #:a-real                    3.3
   #:a-string                  "thrstr"
   #:a-symbol                  'thrsym
   #:a-10-char-string          "thrstrthr3"
   #:a-10-char-symbol          'thrsymthr3
   #:a-time-utc                (date->time-utc (make-date 0 3 3 3 3 3 2003 3))
   #:a-time-tai                (date->time-tai (make-date 0 33 33 3 3 3 2033 3))
   #:a-short-enum              (short-enum c)
   #:a-long-enum               (long-enum d)
   #:a-post                    post
   #:a-required-integer        4
   #:a-required-real           4.4
   #:a-required-string         "frstr"
   #:a-required-symbol         'frsym
   #:a-required-10-char-string "foustrfou4"
   #:a-required-10-char-symbol 'fousymfou4
   #:a-required-time-utc       (date->time-utc (make-date 0 4 4 4 4 4 2004 4))
   #:a-required-time-tai       (date->time-tai (make-date 0 44 44 4 4 4 2044 4))
   #:a-required-short-enum     (short-enum a)
   #:a-required-long-enum      (long-enum b)
   #:a-required-post           post))

; Tests ------------------------------------------

(define/provide-test-suite update-tests
  #:before (cut recreate-tables)
  #:after  (cut recreate-tables)
  
  (test-suite "kitchen sink update page 1 (entity-data-attributes)"
    (let ([sink  (save! (make-sink (save! (make-first-post (save! (make-dave))))))]
          [sink2 (make-sink-2 (make-second-post (make-dave)))]) ; for comparison after update
      (test-case "update page displays with correct attributes"
        (open/wait (format "/sinks/~a/edit" (snooze-struct-id sink)))
        (check-equal? (title-ref) (format "Edit kitchen sink: #(struct:kitchen-sink ~a ~a)"
                                          (snooze-struct-id       sink)
                                          (snooze-struct-revision sink)))
        (let ([table (node/tag 'table)])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) (length (entity-data-attributes kitchen-sink)))
          ; check attribute labels
          (for ([attr (in-list (entity-data-attributes kitchen-sink))]
                [row  (in-naturals)])
            (check-equal? (inner-html-ref (node/cell/xy 0 row table))
                          (string-append (xml+quotable->string (attribute-label-xml attr))
                                         (if (type-allows-null? (attribute-type attr))
                                             ""
                                             " (required)"))))))
      
      (test-case "make updates and check correct values"
        (open/wait (format "/sinks/~a/edit" (snooze-struct-id sink)))
        
        (let ([table (node/tag 'table)])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) (length (entity-data-attributes kitchen-sink)))
          ; update attributes, one at a time
          (click (node/jquery ":checkbox" (node/cell/xy 1 0 table)))
          (for ([test-attr (in-list (entity-data-attributes kitchen-sink))])
            (check-equal? (snooze-struct-ref sink test-attr) (snooze-struct-ref sink2 test-attr)))))))
  
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

