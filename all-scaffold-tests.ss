#lang scheme/base

(require "test-base.ss")

(require "attribute-editor-test.ss"
         "compound-editor-test.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-scaffold-tests
  attribute-editor-tests
  compound-editor-tests)
