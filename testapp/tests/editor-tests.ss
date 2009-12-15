#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss")

; Helpers ----------------------------------------


; Tests ------------------------------------------

(define editor-tests
  (test-suite "editor"
    (test-case "test-page displays"
      (open/wait "/editor")
      (check-equal? (title-ref) "New kitchen sink"))))

; Provide statements -----------------------------

(provide editor-tests)

