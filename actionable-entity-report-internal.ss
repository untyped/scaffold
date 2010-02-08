#lang scheme/base

(require "base.ss")

; Structure types --------------------------------

; (struct symbol string (listof string))
(define-struct report-action (id string classes) #:transparent)

; symbol string (listof string) -> report-action
(define (create-report-action id str [classes null])
  (make-report-action id str classes))

; Provide statements -----------------------------

(provide/contract 
 [struct report-action ([id symbol?] [string string?] [classes (listof string?)])]
 [create-report-action (->* (symbol? string?) ((listof string?)) report-action?)])
