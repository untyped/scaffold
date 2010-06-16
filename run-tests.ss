#lang scheme/base

(require "test-base.ss")

(require "all-scaffold-tests.ss")

; Main program body ----------------------------

(error-print-width 120)
(print-struct #t)
(dev? #t)

(run-tests all-scaffold-tests)
