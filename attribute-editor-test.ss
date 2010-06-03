#lang scheme/base

(require "test-base.ss")

(require "attribute-editor.ss")

; Helpers ----------------------------------------

(define-entity foo ([okay? boolean]))

; Tests ------------------------------------------

(define/provide-test-suite attribute-editor-tests
  
  (test-case "attribute->id"
    (check-equal? (attribute->id (attr foo okay?)) 'foo-okay-field)
    (check-equal? (attribute->id (attr foo okay?) "-results") 'foo-okay-results)))

