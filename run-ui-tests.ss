#lang scheme/base

(require "base.ss")

(require (delirium-in)
         (schemeunit-in text-ui)
         (smoke-in)
         "testapp/all-testapp-tests.ss"
         "testapp/base.ss"
         "testapp/site.ss"
         "testapp/content/content.ss")

; Main program body ----------------------------

(error-print-width 120)
(print-struct #t)
(dev? #t)

(serve/smoke/delirium
 (cut site-dispatch test-site (current-request))
 all-testapp-tests
 #:htdocs-paths (list testapp-htdocs-path))
