#lang scheme/base

(require "base.ss")

(require srfi/19
         (delirium-in)
         (schemeunit-in main util)
         (unlib-in time)
         "testapp/site.ss")

; Procedures -------------------------------------

; string [string] -> time-utc
(define (st-utc str [format-str "~Y-~m-~d ~H:~M"])
  (date->time-utc (string->date str format-str)))

; string [string] -> time-tai
(define (st-tai str [format-str "~Y-~m-~d ~H:~M"])
  (date->time-tai (string->date str format-str)))

; time-utc [string] -> string
(define (ts-utc time [format-str "~Y-~m-~d ~H:~M"])
  (date->string (time-utc->date time) format-str))

; time-tai [string] -> string
(define (ts-tai time [format-str "~Y-~m-~d ~H:~M"])
  (date->string (time-tai->date time) format-str))

; xml_quotable -> string
(define (xml+quotable->string val)
  (xml->string (xml-quote val)))

; Provide statements -----------------------------

(provide (delirium-out))
(provide (schemeunit-out main util))
(provide (all-from-out "base.ss"))
(provide (all-from-out "testapp/site.ss"))

(provide/contract
 [st-utc (->* (string?)   (string?) time-utc?)]
 [st-tai (->* (string?)   (string?) time-tai?)]
 [ts-utc (->* (time-utc?) (string?) string?)]
 [ts-tai (->* (time-tai?) (string?) string?)]
 [xml+quotable->string (-> xml+quotable? string?)])
