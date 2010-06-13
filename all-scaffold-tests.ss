#lang scheme/base

(require "test-base.ss")

(require "report-util-test.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-scaffold-tests
  report-util-tests)
