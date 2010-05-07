#lang scheme/base

(require "base.ss")

(require (delirium-in)
         (smoke-in)
         "testapp/content-base.ss"
         "testapp/content/content.ss"
         "testapp/main.ss"
         "testapp/all-tests.ss")

; Main program body ----------------------------

(error-print-width 120)
(print-struct #t)
(dev? #t)

(serve/smoke/delirium
  (lambda ()
    (with-connection
      (site-dispatch test-site (current-request))))
 all-testapp-tests
 #:htdocs-paths (list testapp-htdocs-path))
