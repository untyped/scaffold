#lang scheme/base

(require "test-base.ss")

(require (delirium-in)
         (schemeunit-in text-ui)
         (smoke-in)
         "testapp/content-base.ss"
         "testapp/content/content.ss"
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
 #:htdocs-paths
 (list testapp-htdocs-path)
 #:run-tests
 (lambda (tests)
   (with-connection
     (run-tests tests))))
