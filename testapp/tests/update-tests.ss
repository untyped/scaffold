#lang scheme/base

(require srfi/13
         srfi/19
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss"
         "../content-base.ss"
         "../site.ss")

; Helpers ----------------------------------------

; time -> string
(define utc->str (cut ts-utc <> "~d/~m/~Y ~H:~M"))
(define tai->str (cut ts-tai <> "~d/~m/~Y ~H:~M"))

; any -> string
(define fmt    (cut format "~a" <>))

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
  ;#:after  (cut recreate-tables)
  
  (test-suite "kitchen sink update page 1 (entity-data-attributes)"
    #:before (cut recreate-tables)
    (let* ([user  (save! (make-dave))]
           [sink  (save! (make-sink (save! (make-first-post user))))]
           [sink2 (make-sink-2 (save! (make-second-post user)))]) ; for comparison after update
      (test-case "update page displays with correct attributes"
        (open/wait (controller-url sink-update/vanilla sink))
        (check-equal? (title-ref) "A vanilla entity-editor")
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
                                             " (required)"))))
          (let* ([mkrow   (cut node/cell/xy 1 <> table)]
                 [row-val (lambda (row sel) (js-ref (!dot ($ ,(node/jquery sel (mkrow row))) (val))))])
            ; check attribute populated values, one at a time
            (check-equal? (node-count (node/jquery ":checked" (mkrow 0))) 0)
            (check-equal? (row-val 1  "input")          (fmt (kitchen-sink-a-integer        sink)))
            (check-equal? (row-val 2  "input")          (fmt (kitchen-sink-a-real           sink)))
            (check-equal? (row-val 3  "textarea")       (fmt (kitchen-sink-a-string         sink)))
            (check-equal? (row-val 4  "textarea")       (fmt (kitchen-sink-a-symbol         sink)))
            (check-equal? (row-val 5  "input")          (fmt (kitchen-sink-a-10-char-string sink)))
            (check-equal? (row-val 6  "input")          (fmt (kitchen-sink-a-10-char-symbol sink)))
            (check-equal? (row-val 7  "input")          (utc->str (kitchen-sink-a-time-utc sink)))
            (check-equal? (row-val 8  "input")          (tai->str (kitchen-sink-a-time-tai sink)))
            (check-equal? (row-val 9  ":radio:checked") (fmt (kitchen-sink-a-short-enum sink)))
            (check-equal? (row-val 10 "select")         (fmt (kitchen-sink-a-long-enum sink)))
            (check-equal? (row-val 11 "select")         "") ; no post selected, so #f == ""
            (check-equal? (row-val 12 "input")          (fmt (kitchen-sink-a-required-integer        sink)))
            (check-equal? (row-val 13 "input")          (fmt (kitchen-sink-a-required-real           sink)))
            (check-equal? (row-val 14 "textarea")       (fmt (kitchen-sink-a-required-string         sink)))
            (check-equal? (row-val 15 "textarea")       (fmt (kitchen-sink-a-required-symbol         sink)))
            (check-equal? (row-val 16 "input")          (fmt (kitchen-sink-a-required-10-char-string sink)))
            (check-equal? (row-val 17 "input")          (fmt (kitchen-sink-a-required-10-char-symbol sink)))
            (check-equal? (row-val 18 "input")          (utc->str (kitchen-sink-a-required-time-utc sink)))
            (check-equal? (row-val 19 "input")          (tai->str (kitchen-sink-a-required-time-tai sink)))
            (check-equal? (row-val 20 ":radio:checked") (fmt (kitchen-sink-a-required-short-enum sink)))
            (check-equal? (row-val 21 "select")         (fmt (kitchen-sink-a-required-long-enum sink)))
            (check-equal? (row-val 22 "select")         (fmt (snooze-struct-id (kitchen-sink-a-required-post sink)))))))
      
      (test-case "make updates and check correct values"
        (open/wait (controller-url sink-update/vanilla sink))
        (let* ([table (node/tag 'table)])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) (length (entity-data-attributes kitchen-sink)))
          (let* ([mkrow  (cut node/cell/xy 1 <> table)]
                 [field  (lambda (row sel) (node/jquery sel (mkrow row)))])
            ; update attributes, one at a time
            (click      (field 0  ":checkbox")) ; check "a boolean"
            (enter-text (field 1  "input")    (fmt (kitchen-sink-a-integer        sink2)))
            (enter-text (field 2  "input")    (fmt (kitchen-sink-a-real           sink2)))
            (enter-text (field 3  "textarea") (fmt (kitchen-sink-a-string         sink2)))
            (enter-text (field 4  "textarea") (fmt (kitchen-sink-a-symbol         sink2)))
            (enter-text (field 5  "input")    (fmt (kitchen-sink-a-10-char-string sink2)))
            (enter-text (field 6  "input")    (fmt (kitchen-sink-a-10-char-symbol sink2)))
            (enter-text (field 7  "input")    (utc->str (kitchen-sink-a-time-utc    sink2)))
            (enter-text (field 8  "input")    (tai->str (kitchen-sink-a-time-tai    sink2)))
            (click      (field 9  (format ":radio[value='~a']" (kitchen-sink-a-short-enum sink2))))
            (select     (field 10 "select")   (kitchen-sink-a-long-enum sink2))
            (select     (field 11 "select")   (fmt (snooze-struct-id (kitchen-sink-a-post  sink2))))
            (enter-text (field 12 "input")    (fmt (kitchen-sink-a-required-integer        sink2)))
            (enter-text (field 13 "input")    (fmt (kitchen-sink-a-required-real           sink2)))
            (enter-text (field 14 "textarea") (fmt (kitchen-sink-a-required-string         sink2)))
            (enter-text (field 15 "textarea") (fmt (kitchen-sink-a-required-symbol         sink2)))
            (enter-text (field 16 "input")    (fmt (kitchen-sink-a-required-10-char-string sink2)))
            (enter-text (field 17 "input")    (fmt (kitchen-sink-a-required-10-char-symbol sink2)))
            (enter-text (field 18 "input")    (utc->str (kitchen-sink-a-required-time-utc    sink2)))
            (enter-text (field 19 "input")    (tai->str (kitchen-sink-a-required-time-tai    sink2)))
            (click      (field 20 (format ":radio[value='~a']" (kitchen-sink-a-required-short-enum sink2))))
            (select     (field 21 "select")   (kitchen-sink-a-required-long-enum sink2))
            (select     (field 22 "select")   (fmt (snooze-struct-id (kitchen-sink-a-required-post sink2))))
            ; submit the data
            (click/wait (node/jquery ":submit"))
            (let ([new-sink (find-by-id kitchen-sink (snooze-struct-id sink))])
              (for ([test-attr (in-list (entity-data-attributes kitchen-sink))])
                (check-equal? (snooze-struct-ref new-sink test-attr) 
                              (snooze-struct-ref sink2    test-attr)
                              (format "Attribute does not match ~a" (attribute-name test-attr))))))))))
  
  (test-suite "kitchen sink update page 2 (subset of data attributes)"
    #:before (cut recreate-tables)
    (let* ([user  (save! (make-dave))]
           [sink  (save! (make-sink (save! (make-first-post user))))]
           [sink2 (make-sink-2 (save! (make-second-post user)))]) ; for comparison after update
      (test-case "update page displays with correct attributes"
        (open/wait (controller-url sink-update/attrs sink))
        (check-equal? (title-ref) "An entity-editor with custom attributes")
        (let ([table (node/tag 'table)])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) 3)
          ; check attribute labels
          (for ([attr (in-list (attr-list kitchen-sink a-boolean a-real a-integer))]
                [row  (in-naturals)])
            (check-equal? (inner-html-ref (node/cell/xy 0 row table))
                          (string-append (xml+quotable->string (attribute-label-xml attr))
                                         (if (type-allows-null? (attribute-type attr))
                                             ""
                                             " (required)"))))
          (let* ([mkrow   (cut node/cell/xy 1 <> table)]
                 [row-val (lambda (row sel) (js-ref (!dot ($ ,(node/jquery sel (mkrow row))) (val))))])
            ; check attribute populated values, one at a time
            (check-equal? (node-count (node/jquery ":checked" (mkrow 0))) 0)
            (check-equal? (row-val 1 "input") (fmt (kitchen-sink-a-real    sink)))
            (check-equal? (row-val 2 "input") (fmt (kitchen-sink-a-integer sink))))))
      
      (test-case "make updates and check correct values"
        (open/wait (controller-url sink-update/attrs sink))
        
        (let* ([table (node/tag 'table)])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) 3)
          (let* ([mkrow  (cut node/cell/xy 1 <> table)]
                 [field  (lambda (row sel) (node/jquery sel (mkrow row)))])
            ; update attributes, one at a time
            (click      (field 0 ":checkbox")) ; check "a boolean"
            (enter-text (field 1 "input") (fmt (kitchen-sink-a-real    sink2)))
            (enter-text (field 2 "input") (fmt (kitchen-sink-a-integer sink2)))
            ; submit the data
            (click/wait (node/jquery ":submit"))
            (let ([new-sink (find-by-id kitchen-sink (snooze-struct-id sink))])
              ; check updates have taken place for specified attributes ...
              (for ([test-attr (in-list (attr-list kitchen-sink a-boolean a-real a-integer))])
                (check-equal? (snooze-struct-ref new-sink test-attr) 
                              (snooze-struct-ref sink2    test-attr)
                              (format "Attribute does not match ~a" (attribute-name test-attr))))
              ; ... but that none of the other attributes have been changed
              (for ([test-attr (in-list (filter (lambda (att)
                                                  (not (memq att (attr-list kitchen-sink a-boolean a-real a-integer))))
                                                (entity-data-attributes kitchen-sink)))])
                (check-equal? (snooze-struct-ref new-sink test-attr) 
                              (snooze-struct-ref sink     test-attr)
                              (format "Attribute should not have changed ~a" (attribute-name test-attr))))))))))
  
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

