#lang scheme/base

(require "test-base.ss")

(require "report-tests.ss"
         "testapp/all-tests.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-testapp-tests
  all-testapp-tests
  report-tests)
