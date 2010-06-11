#lang scheme/base

(require "test-base.ss")

(require (unlib-in date)
         "report-util.ss")

; Helpers ----------------------------------------

(define (st str)
  (date->time-utc (string->date str "~Y-~m-~d ~H:~M")))

(define (ts time)
  (date->string (time-utc->date time) "~Y-~m-~d ~H:~M"))

; Tests ------------------------------------------

(define/provide-test-suite report-util-tests
  
  (test-case "pattern->regexp"
    (check-equal? (pattern->regexp "dave")     "^dave")
    (check-equal? (pattern->regexp "dave"  #t) "dave")
    (check-equal? (pattern->regexp "dave*here" #t) "dave.*here")
    (check-equal? (pattern->regexp "dave?here" #t) "dave.here")
    (check-equal? (pattern->regexp "dave\\here" #t) "davehere")
    (check-equal? (pattern->regexp "dave\\*here" #t) "dave\\*here")
    (check-equal? (pattern->regexp "dave\\?here" #t) "dave\\?here")
    (check-equal? (pattern->regexp "dave\\\\here" #t) "dave\\\\here"))
  
  (test-case "pattern->time"
    (let ([now (st "2010-03-28 09:00")])
      (check-equal? (ts (pattern->time "today"     #:now (st "2010-03-28 00:00"))) "2010-03-28 00:00")
      (check-equal? (ts (pattern->time "yesterday" #:now (st "2010-03-28 00:00"))) "2010-03-27 00:00")
      (check-equal? (ts (pattern->time "mon"       #:now (st "2010-03-23 00:00"))) "2010-03-22 00:00")
      (check-equal? (ts (pattern->time "wed"       #:now (st "2010-03-23 00:00"))) "2010-03-17 00:00")
      (check-equal? (ts (pattern->time "fri"       #:now (st "2010-03-23 00:00"))) "2010-03-19 00:00")
      (check-equal? (ts (pattern->time "monday"    #:now (st "2010-03-23 00:00"))) "2010-03-22 00:00")
      (check-equal? (ts (pattern->time "wednesday" #:now (st "2010-03-23 00:00"))) "2010-03-17 00:00")
      (check-equal? (ts (pattern->time "friday"    #:now (st "2010-03-23 00:00"))) "2010-03-19 00:00")
      (check-equal? (ts (pattern->time "2/3"       #:now (st "2010-03-28 00:00"))) "2010-03-02 00:00")
      (check-equal? (ts (pattern->time "2/4"       #:now (st "2010-03-28 00:00"))) "2009-04-02 00:00")
      (check-equal? (ts (pattern->time "2:3"       #:now (st "2010-03-28 00:00"))) "2010-03-02 02:03")
      (check-equal? (ts (pattern->time "10:4"      #:now (st "2010-03-27 00:00"))) "2009-04-02 10:04")
      (check-false (pattern->time "omg" #:now (st "2010-03-27 00:00"))))))