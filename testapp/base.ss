#lang scheme

(require scheme/runtime-path
         "../base.ss"
         "../main.ss")

; Configuration ----------------------------------

(define-runtime-path testapp-htdocs-path "htdocs")

; Provide statements -----------------------------

(provide (all-from-out "../base.ss"))
(provide (all-from-out "../main.ss"))

(provide/contract
 [testapp-htdocs-path path?])
