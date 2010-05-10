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
   #:a-time-utc                (date->time-utc (make-date 0 0 1 1 1 1 2001 0))
   #:a-time-tai                (date->time-tai (make-date 0 0 11 11 11 11 2011 0))
   #:a-short-enum              (short-enum a)
   #:a-long-enum               (long-enum b)
   #:a-post                    #f
   #:a-required-integer        2
   #:a-required-real           2.2
   #:a-required-string         "twostr"
   #:a-required-symbol         'twosym
   #:a-required-10-char-string "twostrtwo2"
   #:a-required-10-char-symbol 'twosymtwo2
   #:a-required-time-utc       (date->time-utc (make-date 0 0 2 2 2 2 2002 0))
   #:a-required-time-tai       (date->time-tai (make-date 0 0 22 22 22 2 2022 0))
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
   #:a-time-utc                (date->time-utc (make-date 0 0 3 3 3 3 2003 0))
   #:a-time-tai                (date->time-tai (make-date 0 0 33 3 3 3 2033 0))
   #:a-short-enum              (short-enum c)
   #:a-long-enum               (long-enum d)
   #:a-post                    post
   #:a-required-integer        4
   #:a-required-real           4.4
   #:a-required-string         "frstr"
   #:a-required-symbol         'frsym
   #:a-required-10-char-string "foustrfou4"
   #:a-required-10-char-symbol 'fousymfou4
   #:a-required-time-utc       (date->time-utc (make-date 0 0 4 4 4 4 2004 0))
   #:a-required-time-tai       (date->time-tai (make-date 0 0 44 4 4 4 2044 0))
   #:a-required-short-enum     (short-enum a)
   #:a-required-long-enum      (long-enum b)
   #:a-required-post           post))

; Tests ------------------------------------------

(define/provide-test-suite update-tests
  #:before (cut recreate-tables)
  #:after  (cut recreate-tables)
  
  (test-suite "kitchen sink update page 1 (entity-data-attributes)"
    (let* ([user  (save! (make-dave))]
           [sink  (save! (make-sink (save! (make-first-post user))))]
           [sink2 (make-sink-2 (save! (make-second-post user)))]) ; for comparison after update
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
        
        (let* ([table (node/tag 'table)])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) (length (entity-data-attributes kitchen-sink)))
          (let ([mkrow  (cut node/cell/xy 1 <> table)]
                [fmt    (cut format "~a" <>)]
                [ts-utc (cut ts-utc <> "~d/~m/~Y ~H:~M")]
                [ts-tai (cut ts-tai <> "~d/~m/~Y ~H:~M")])
            ; update attributes, one at a time
            (click      (node/jquery ":checkbox" (mkrow 0))) ; check "a boolean"
            (enter-text (node/jquery "input"     (mkrow 1)) (fmt (kitchen-sink-a-integer        sink2)))
            (enter-text (node/jquery "input"     (mkrow 2)) (fmt (kitchen-sink-a-real           sink2)))
            (enter-text (node/jquery "textarea"  (mkrow 3)) (fmt (kitchen-sink-a-string         sink2)))
            (enter-text (node/jquery "textarea"  (mkrow 4)) (fmt (kitchen-sink-a-symbol         sink2)))
            (enter-text (node/jquery "input"     (mkrow 5)) (fmt (kitchen-sink-a-10-char-string sink2)))
            (enter-text (node/jquery "input"     (mkrow 6)) (fmt (kitchen-sink-a-10-char-symbol sink2)))
            (enter-text (node/jquery "input"     (mkrow 7)) (ts-utc (kitchen-sink-a-time-utc sink2)))
            (enter-text (node/jquery "input"     (mkrow 8)) (ts-tai (kitchen-sink-a-time-tai sink2)))
            (click      (node/jquery (format ":radio[value='~a']" (kitchen-sink-a-short-enum sink2)) (mkrow 9)))
            (select     (node/jquery "select"    (mkrow 10)) (kitchen-sink-a-long-enum sink2))
            (select     (node/jquery "select"    (mkrow 11)) (fmt (snooze-struct-id (kitchen-sink-a-post sink2))))
            (enter-text (node/jquery "input"     (mkrow 12)) (fmt (kitchen-sink-a-required-integer        sink2)))
            (enter-text (node/jquery "input"     (mkrow 13)) (fmt (kitchen-sink-a-required-real           sink2)))
            (enter-text (node/jquery "textarea"  (mkrow 14)) (fmt (kitchen-sink-a-required-string         sink2)))
            (enter-text (node/jquery "textarea"  (mkrow 15)) (fmt (kitchen-sink-a-required-symbol         sink2)))
            (enter-text (node/jquery "input"     (mkrow 16)) (fmt (kitchen-sink-a-required-10-char-string sink2)))
            (enter-text (node/jquery "input"     (mkrow 17)) (fmt (kitchen-sink-a-required-10-char-symbol sink2)))
            (enter-text (node/jquery "input"     (mkrow 18)) (ts-utc (kitchen-sink-a-required-time-utc sink2)))
            (enter-text (node/jquery "input"     (mkrow 19)) (ts-tai (kitchen-sink-a-required-time-tai sink2)))
            (click      (node/jquery (format ":radio[value='~a']" (kitchen-sink-a-required-short-enum sink2)) (mkrow 20)))
            (select     (node/jquery "select"    (mkrow 21)) (kitchen-sink-a-required-long-enum sink2))
            (select     (node/jquery "select"    (mkrow 22)) (fmt (snooze-struct-id (kitchen-sink-a-required-post sink2))))
            ; submit the data
            (click/wait (node/jquery ":submit"))
            (let ([new-sink (find-by-id kitchen-sink (snooze-struct-id sink))])
              (for ([test-attr (in-list (entity-data-attributes kitchen-sink))])
                (check-equal? (snooze-struct-ref new-sink test-attr) 
                              (snooze-struct-ref sink2    test-attr)
                              (format "Attribute does not match ~a" (attribute-name test-attr))))))))))
  
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

