#lang scheme/base

(require "../base.ss")

(require srfi/19
         "db.ss")

; Enumerations -----------------------------------

(define-enum short-enum
  ([a _ "first option"]
   [b _ "second option"]
   [c _ "third option"]
   [d _ "fourth option"]))

(define-enum long-enum
  ([a _ "first option"]
   [b _ "second option"]
   [c _ "third option"]
   [d _ "fourth option"]
   [e _ "fifth option"]
   [f _ "getting boring"]
   [g _ "really boring"]
   [h _ "are we done yet"]))

; Entities ---------------------------------------

(define-entity user
  ([username  string #:allow-null? #f #:max-length 32]
   [surname   string #:allow-null? #f #:max-length 128]
   [forenames string #:allow-null? #f #:max-length 128]
   [email     string #:allow-null? #f #:max-length 1024]
   [created   time-utc #:allow-null? #f #:default-proc (cut current-time time-utc)])
  #:pretty-formatter 
  (lambda (usr)
    (format "~a ~a" (user-forenames usr) (user-surname usr))))

(define-entity post
  ([user    user   #:allow-null? #f]
   [subject string #:allow-null? #f #:max-length 128]
   [content string])
  #:pretty-formatter
  (lambda (post)
    (post-subject post)))

(define-entity comment
  ([user    user #:allow-null? #f]
   [post    post #:allow-null? #f]
   [content string])
  #:pretty-formatter
  (lambda (comment)
    (comment-content comment)))

(define-entity tag
  ([text string #:allow-null? #f #:max-length 32])
  #:pretty-formatter
  (lambda (tg)
    (tag-text tg)))

(define-entity tagging
  ([post post #:allow-null? #f]
   [tag  tag  #:allow-null? #f]))

(define-entity kitchen-sink
  ([a-boolean                 boolean]
   [a-integer                 integer]
   [a-real                    real]
   [a-string                  string]
   [a-symbol                  symbol]
   [a-10-char-string          string    #:max-length 10]
   [a-10-char-symbol          symbol    #:max-length 10]
   [a-time-utc                time-utc]
   [a-time-tai                time-tai]
   [a-short-enum              enum      #:values short-enum]
   [a-long-enum               enum      #:values long-enum]
   [a-post                    post]
   [a-required-integer        integer   #:allow-null? #f]
   [a-required-real           real      #:allow-null? #f]
   [a-required-string         string    #:allow-null? #f]
   [a-required-symbol         symbol    #:allow-null? #f]
   [a-required-10-char-string string    #:allow-null? #f #:max-length 10]
   [a-required-10-char-symbol symbol    #:allow-null? #f #:max-length 10]
   [a-required-time-utc       time-utc  #:allow-null? #f]
   [a-required-time-tai       time-tai  #:allow-null? #f]
   [a-required-short-enum     enum      #:allow-null? #f #:values short-enum]
   [a-required-long-enum      enum      #:allow-null? #f #:values long-enum]
   [a-required-post           post      #:allow-null? #f]))

; -> void
(define (recreate-tables)
  (let ([tables (list user post comment tag tagging kitchen-sink)])
    (for-each drop-table (reverse tables))
    (for-each create-table tables)))

; Provides ---------------------------------------

(provide short-enum
         long-enum)

(provide/contract/entities
 [entity user]
 [entity post]
 [entity comment]
 [entity tag]
 [entity tagging]
 [entity kitchen-sink]
 [recreate-tables (-> void?)])
