#lang scheme/base

(require "../test-base.ss")

(require "tests/review-tests.ss"
         "tests/update-tests.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-testapp-tests
  review-tests
  update-tests)
