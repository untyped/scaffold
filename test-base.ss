#lang scheme/base

(require "base.ss")

(require srfi/19
         (delirium-in)
         (schemeunit-in main util)
         (unlib-in time)
         "testapp/site.ss")

; Procedures -------------------------------------

; string -> time-utc
(define (st-utc str)
  (date->time-utc (string->date str "~Y-~m-~d ~H:~M")))

; string -> time-tai
(define (st-tai str)
  (date->time-tai (string->date str "~Y-~m-~d ~H:~M")))

; time-utc -> string
(define (ts-utc time)
  (date->string (time-utc->date time) "~Y-~m-~d ~H:~M"))

; time-tai -> string
(define (ts-tai time)
  (date->string (time-tai->date time) "~Y-~m-~d ~H:~M"))

; xml_quotable -> string
(define (xml+quotable->string val)
  (xml->string (xml-quote val)))

; Provide statements -----------------------------

(provide (delirium-out)
         (schemeunit-out main util)
         (all-from-out "base.ss"
                       "testapp/site.ss"))

(provide/contract
 [st-utc (-> string? time-utc?)]
 [st-tai (-> string? time-tai?)]
 [ts-utc (-> time-utc? string?)]
 [ts-tai (-> time-tai? string?)]
 [xml+quotable->string (-> xml+quotable? string?)])
