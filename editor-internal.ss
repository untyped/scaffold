#lang scheme/base

(require "base.ss")

(require "util.ss")

; Interfaces -------------------------------------

(define editor<%>
  (interface ()
    value-changed? ; -> boolean
    parse          ; -> (listof check-result)
    validate))     ; -> (listof check-result)

; Provide statements -----------------------------

(provide editor<%>)
