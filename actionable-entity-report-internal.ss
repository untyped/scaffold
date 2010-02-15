#lang scheme/base

(require "base.ss")

; Structure types --------------------------------

; (struct symbol xml (listof string))
(define-struct report-action (id xml classes) #:transparent)

; symbol string (listof string) -> report-action
(define (create-report-action id str [classes null])
  (make-report-action id str classes))

; Provide statements -----------------------------

(provide/contract 
 [struct report-action ([id symbol?] [xml xml+quotable?] [classes (listof string?)])]
 [create-report-action (->* (symbol? xml+quotable?) ((listof string?)) report-action?)])
