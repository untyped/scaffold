#lang scheme/base

(require "base.ss")

(require "util.ss")

; Interfaces -------------------------------------

(define editor<%>
  (interface ()
    parse      ; -> (listof check-result)
    validate)) ; -> (listof check-result)

; Provide statements -----------------------------

(provide editor<%>)
