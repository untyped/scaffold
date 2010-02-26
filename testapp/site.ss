#lang scheme/base

(require "../base.ss")

(require (dispatch-in))

; Site -------------------------------------------

(define-site test-site
  ([("")        home]
   [("/editor") editor]
   [("/review") review])
  #:requestless? #t)

; Controllers ------------------------------------

(define-controller (home)
  (make-html-response
   (xml (html (head (title "Test application"))
              (body (h1 "Test application")
                    (ul ,@(reverse (for/list ([controller (site-controllers test-site)])
                                     (with-handlers ([exn? (lambda _ (xml))])
                                       (if (eq? controller home)
                                           (xml)
                                           (xml (li (a (@ [href ,(controller-url controller)])
                                                       ,(controller-id controller))))))))))))))

; Provide statements -----------------------------

(provide (site-out test-site))
