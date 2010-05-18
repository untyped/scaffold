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
(define fmt (cut format "~a" <>))

; -> person
(define (make-dave)
  (make-user/defaults #:username  "david"
                      #:surname   "brooks"
                      #:forenames "david james"
                      #:email     "djb@example.com"))

; person string -> post
(define (make-test-post user subject) 
  (make-post/defaults #:user user #:subject subject))

; -> kitchen-sink
(define (make-sink-1 post)  
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

; js -> (U string #f)
(define (field-value-ref sel)
  (js-ref (!dot ($ (!index ,sel 0)) (val)))) 

; Tests ------------------------------------------

(define/provide-test-suite update-tests
  
  (test-suite "kitchen sink update page 1 (entity-data-attributes)"
    #:before (cut recreate-tables)
    (let* ([user  (save! (make-dave))]
           [sink  (save! (make-sink-1 (save! (make-test-post user "my first post"))))]
           [sink2 (make-sink-2 (save! (make-test-post user "my second post")))]) ; for comparison after update
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
                                         (if (type-allows-null? (attribute-type attr)) "" " (required)"))))
          ; check attribute populated values, one at a time
          (let* ([mkrow   (cut node/cell/xy 1 <> table)]
                 [row-val (lambda (row sel) (js-ref (!dot ($ ,(node/jquery sel (mkrow row))) (val))))])
            (check-equal? (node-count (node/jquery ":checked" (mkrow 0))) 0)
            (check-equal? (row-val 1  "input")          (fmt (kitchen-sink-a-integer        sink)))
            (check-equal? (row-val 2  "input")          (fmt (kitchen-sink-a-real           sink)))
            (check-equal? (row-val 3  "textarea")       (fmt (kitchen-sink-a-string         sink)))
            (check-equal? (row-val 4  "textarea")       (fmt (kitchen-sink-a-symbol         sink)))
            (check-equal? (row-val 5  "input")          (fmt (kitchen-sink-a-10-char-string sink)))
            (check-equal? (row-val 6  "input")          (fmt (kitchen-sink-a-10-char-symbol sink)))
            (check-equal? (row-val 7  "input")          (utc->str (kitchen-sink-a-time-utc  sink)))
            (check-equal? (row-val 8  "input")          (tai->str (kitchen-sink-a-time-tai  sink)))
            (check-equal? (row-val 9  ":radio:checked") (fmt (kitchen-sink-a-short-enum     sink)))
            (check-equal? (row-val 10 "select")         (fmt (kitchen-sink-a-long-enum      sink)))
            (check-equal? (row-val 11 "select")         "") ; no post selected, so #f == ""
            (check-equal? (row-val 12 "input")          (fmt (kitchen-sink-a-required-integer        sink)))
            (check-equal? (row-val 13 "input")          (fmt (kitchen-sink-a-required-real           sink)))
            (check-equal? (row-val 14 "textarea")       (fmt (kitchen-sink-a-required-string         sink)))
            (check-equal? (row-val 15 "textarea")       (fmt (kitchen-sink-a-required-symbol         sink)))
            (check-equal? (row-val 16 "input")          (fmt (kitchen-sink-a-required-10-char-string sink)))
            (check-equal? (row-val 17 "input")          (fmt (kitchen-sink-a-required-10-char-symbol sink)))
            (check-equal? (row-val 18 "input")          (utc->str (kitchen-sink-a-required-time-utc  sink)))
            (check-equal? (row-val 19 "input")          (tai->str (kitchen-sink-a-required-time-tai  sink)))
            (check-equal? (row-val 20 ":radio:checked") (fmt (kitchen-sink-a-required-short-enum     sink)))
            (check-equal? (row-val 21 "select")         (fmt (kitchen-sink-a-required-long-enum      sink)))
            (check-equal? (row-val 22 "select")         (fmt (snooze-struct-id (kitchen-sink-a-required-post sink)))))))
      
      (test-case "parse errors"
        (open/wait (controller-url sink-update/vanilla sink))
        (let* ([table (node/tag 'table)]
               [mkrow (cut node/cell/xy 1 <> table)]
               [field (lambda (row [sel ":input"]) (node/jquery sel (mkrow row)))])
          ; Edit attributes that can yield parse errors:
          (enter-text (field 1)  "not an integer")
          (enter-text (field 2)  "not a real")
          (enter-text (field 5)  "not a 10 char string")
          (enter-text (field 6)  "not a 10 char symbol")
          (enter-text (field 7)  "not a time-utc")
          (enter-text (field 8)  "not a time-tai")
          (enter-text (field 12) "not an integer")
          (enter-text (field 13) "not a real")
          (enter-text (field 16) "not a 10 char string")
          (enter-text (field 17) "not a 10 char symbol")
          (enter-text (field 18) "not a time-utc")
          (enter-text (field 19) "not a time-tai")
          ; submit the data
          (click/wait (node/jquery ":submit"))
          ; check the right error messages appear:
          (check-equal? (text-content-ref (field 1 ".check-label")) "This value must be a whole number.")
          (check-equal? (text-content-ref (field 2 ".check-label")) "This value must be a number.")
          (check-equal? (field-value-ref (field 5 ":input")) "not a 10 c") ; value will be trimmed by maxlength attribute on input tag
          (check-equal? (field-value-ref (field 6 ":input")) "not a 10 c") ; value will be trimmed by maxlength attribute on input tag
          (check-equal? (text-content-ref (field 7 ".check-label")) "Value must be in the format: DD/MM/YYYY HH:MM.")
          (check-equal? (text-content-ref (field 8 ".check-label")) "Value must be in the format: DD/MM/YYYY HH:MM.")
          (check-equal? (text-content-ref (field 12 ".check-label")) "This value must be a whole number.")
          (check-equal? (text-content-ref (field 13 ".check-label")) "This value must be a number.")
          (check-equal? (field-value-ref (field 16 ":input")) "not a 10 c") ; value will be trimmed by maxlength attribute on input tag
          (check-equal? (field-value-ref (field 17 ":input")) "not a 10 c") ; value will be trimmed by maxlength attribute on input tag
          (check-equal? (text-content-ref (field 18 ".check-label")) "Value must be in the format: DD/MM/YYYY HH:MM.")
          (check-equal? (text-content-ref (field 19 ".check-label")) "Value must be in the format: DD/MM/YYYY HH:MM.")
          ; check no other error messages appear:
          (for ([index (in-list (list 0 3 4 9 10 11 14 15 20 21 22))])
            (with-check-info (['index index])
              (check-equal? (text-content-ref (field index ".check-label")) "")))))
      
      (test-case "make updates and check correct values"
        (open/wait (controller-url sink-update/vanilla sink))
        (let* ([table (node/tag 'table)]
               [mkrow (cut node/cell/xy 1 <> table)]
               [field (lambda (row sel) (node/jquery sel (mkrow row)))])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) (length (entity-data-attributes kitchen-sink)))
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
                            (format "Attribute should have been updated ~a" (attribute-name test-attr)))))))))
  
  (test-suite "kitchen sink update page 2 (subset of data attributes)"
    #:before (cut recreate-tables)
    (let* ([user  (save! (make-dave))]
           [sink  (save! (make-sink-1 (save! (make-test-post user "my first post"))))]
           [sink2 (make-sink-2 (save! (make-test-post user "my second post")))]) ; for comparison after update
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
                          (xml+quotable->string (attribute-label-xml attr))))
          ; check attribute populated values, one at a time
          (let* ([mkrow   (cut node/cell/xy 1 <> table)]
                 [row-val (lambda (row sel) (js-ref (!dot ($ ,(node/jquery sel (mkrow row))) (val))))])
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
                              (format "Attribute should have been updated ~a" (attribute-name test-attr))))
              ; ... but that none of the other attributes have been changed
              (for ([test-attr (in-list (filter (lambda (att)
                                                  (not (memq att (attr-list kitchen-sink a-boolean a-real a-integer))))
                                                (entity-data-attributes kitchen-sink)))])
                (check-equal? (snooze-struct-ref new-sink test-attr) 
                              (snooze-struct-ref sink     test-attr)
                              (format "Attribute should not have changed ~a" (attribute-name test-attr))))))))))
  
  (test-suite "kitchen sink attribute fields non-required? page 1 (entity-data-attributes)"
    #:before (cut recreate-tables)
    (let* ([user  (save! (make-dave))]
           [sink  (save! (make-sink-1 (save! (make-test-post user "my first post"))))]
           [sink2 (make-sink-2 (save! (make-test-post user "my second post")))]) ; for comparison after update
      
      (test-case "for non-required attributes, allow empty datum submissions"
        (open/wait (controller-url sink-update/vanilla sink))
        (let* ([table (node/tag 'table)])
          (check-true (node-exists? table))
          (check-equal? (node-count (node/tag 'tr table)) (length (entity-data-attributes kitchen-sink)))
          (let* ([mkrow  (cut node/cell/xy 1 <> table)]
                 [field  (lambda (row sel) (node/jquery sel (mkrow row)))])
            ; update attributes, one at a time
            (enter-text (field 1  "input")    "")
            (enter-text (field 2  "input")    "")
            (enter-text (field 3  "textarea") "")
            (enter-text (field 4  "textarea") "")
            (enter-text (field 5  "input")    "")
            (enter-text (field 6  "input")    "")
            (enter-text (field 7  "input")    "")
            (enter-text (field 8  "input")    "")
            (click      (field 9  (format ":radio[value='~a']" radio-combo-false)))
            (select     (field 10 "select")   combo-box-false)
            (select     (field 11 "select")   combo-box-false)
            ; submit the data
            (click/wait (node/jquery ":submit"))
            (let ([new-sink (find-by-id kitchen-sink (snooze-struct-id sink))])
              (for ([test-attr (in-list (attr-list kitchen-sink
                                                   a-integer a-real 
                                                   a-string a-symbol
                                                   a-10-char-string a-10-char-symbol
                                                   a-time-utc a-time-tai
                                                   a-short-enum a-long-enum
                                                   a-post))])
                (check-false (snooze-struct-ref new-sink test-attr) 
                             (format "Attribute should accept null values ~a" (attribute-name test-attr))))))))))
  
  (test-suite "kitchen sink attribute fields required? page 1 (entity-data-attributes)"
    #:before (cut recreate-tables)
    (let* ([user  (save! (make-dave))]
           [sink  (save! (make-sink-1 (save! (make-test-post user "my first post"))))]
           [sink2 (make-sink-2 (save! (make-test-post user "my second post")))]) ; for comparison after update
      (test-case "for required attributes, empty datum submissions cause warnings"
        (let* ([table (node/tag 'table)]
               [mkrow (cut node/cell/xy 1 <> table)]
               [field (lambda (row sel) (node/jquery sel (mkrow row)))])
          (for ([update-command (in-list (list (lambda () (enter-text (field 12 "input")    ""))
                                               (lambda () (enter-text (field 13 "input")    ""))
                                               (lambda () (enter-text (field 14 "textarea") ""))
                                               (lambda () (enter-text (field 15 "textarea") ""))
                                               (lambda () (enter-text (field 16 "input")    ""))
                                               (lambda () (enter-text (field 17 "input")    ""))
                                               (lambda () (enter-text (field 18 "input")    ""))
                                               (lambda () (enter-text (field 19 "input")    ""))))])
            (open/wait (controller-url sink-update/vanilla sink))
            (let* ([table (node/tag 'table)])
              (check-true (node-exists? table))
              (update-command)
              ; submit the data and verify that the change has not been made
              (click/wait (node/jquery ":submit"))
              (check-equal? (title-ref) "A vanilla entity-editor")
              (let ([new-sink (find-by-id kitchen-sink (snooze-struct-id sink))])
                (for ([test-attr (in-list (entity-data-attributes kitchen-sink))])
                  (check-equal? (snooze-struct-ref new-sink test-attr) 
                                (snooze-struct-ref sink     test-attr)
                                (format "Attribute should not have been updated ~a" (attribute-name test-attr))))))))))))

