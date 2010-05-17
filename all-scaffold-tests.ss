#lang scheme/base

(require "test-base.ss")

(require "compound-editor-test.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-scaffold-tests
  compound-editor-tests)
