#lang scheme/base

(require "test-base.ss")

(require ;"report-test.ss"
         "report-util-test.ss"
         "testapp/all-tests.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-tests
  report-util-tests
  ;report-tests
  all-testapp-tests)
