#lang scheme/base

(require "../test-base.ss")

(require "tests/editor-tests.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-testapp-tests
  editor-tests)
