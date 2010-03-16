#lang scheme/base

(require "../base.ss")

(require (dispatch-in)
         "../arg.ss"
         "model/db.ss"
         "model/entities.ss")

; Site -------------------------------------------

(define-site test-site
  ([("")                                                     home]
   ; post pages
   [("/posts")                                               post-report]
   [("/posts/new")                                           post-create]
   [("/posts/" (entity-arg post))                            post-review]
   [("/posts/" (entity-arg post) "/edit")                    post-update]
   [("/posts/" (entity-arg post) "/delete")                  post-delete]
   ; comment pages
   [("/comments")                                            comment-report]
   [("/comments/new")                                        comment-create]
   [("/comments/" (entity-arg comment))                      comment-review]
   [("/comments/" (entity-arg comment) "/edit")              comment-update]
   [("/comments/" (entity-arg comment) "/delete")            comment-delete]
   ; tag pages
   [("/tags")                                                tag-report]
   [("/tags/new")                                            tag-create]
   [("/tags/" (entity-arg tag))                              tag-review]
   [("/tags/" (entity-arg tag) "/edit")                      tag-update]
   [("/tags/" (entity-arg tag) "/delete")                    tag-delete]
   ; kitchen sink pages
   [("/sinks")                                               sink-report]
   [("/sinks/new")                                           sink-create]
   [("/sinks/" (entity-arg kitchen-sink) "/edit")            sink-update]
   [("/sinks/" (entity-arg kitchen-sink) "/delete")          sink-delete]
   [("/sinks/" (entity-arg kitchen-sink) "/view")            sink-review]
   [("/sinks/" (entity-arg kitchen-sink) "/view-attrs")      sink-review/attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/view-customized") sink-review/customized-attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/view-compound")   sink-review/compound-attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/view-related")    sink-review/related-attrs])
  #:requestless? #t)

; Controllers ------------------------------------

(define-controller (home)
  (make-html-response
   (xml (html (head (title "Test application"))
              (body (h1 "Test application")
                    (ul ,@(for/list ([controller (site-controllers test-site)])
                            (with-handlers ([exn? (lambda _ (xml))])
                              (if (eq? controller home)
                                  (xml)
                                  (xml (li (a (@ [href ,(controller-url controller)])
                                              ,(controller-id controller)))))))))))))

; Provide statements -----------------------------

(provide (site-out test-site))
