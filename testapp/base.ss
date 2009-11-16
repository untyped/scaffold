#lang scheme/base

(require (for-syntax scheme/base)
         scheme/runtime-path
         "../base.ss"
         "../main.ss")

; Configuration ----------------------------------

(define-runtime-path testapp-htdocs-path "htdocs")

; Provide statements -----------------------------

(provide (all-from-out "../base.ss"
                       "../main.ss"))

(provide/contract
 [testapp-htdocs-path path?])
