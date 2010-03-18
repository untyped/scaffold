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
   ; Customised kitchen-sink views
   [("/sinks/" (entity-arg kitchen-sink) "/view-vanilla")    sink-review/vanilla]
   [("/sinks/" (entity-arg kitchen-sink) "/view-attrs")      sink-review/attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/view-customized") sink-review/customized-attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/view-compound")   sink-review/compound-attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/view-related")    sink-review/related-attrs]
   ; Customised kitchen-sink editors
   [("/sinks/" (entity-arg kitchen-sink) "/edit-vanilla")    sink-update/vanilla]
   [("/sinks/" (entity-arg kitchen-sink) "/edit-attrs")      sink-update/attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/edit-customized") sink-update/customized-attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/edit-compound")   sink-update/compound-attrs]
   [("/sinks/" (entity-arg kitchen-sink) "/edit-compound2")  sink-update/compound-attrs2]
   [("/sinks/" (entity-arg kitchen-sink) "/edit-compound3")  sink-update/compound-attrs3]
   [("/sinks/" (entity-arg kitchen-sink) "/edit-related")    sink-update/related-attrs]
   ; Customised kitchen-sink editors (creating)
   [("/sinks/new-vanilla")                                   sink-create/vanilla]
   [("/sinks/new-attrs")                                     sink-create/attrs]
   [("/sinks/new-customized")                                sink-create/customized-attrs]
   [("/sinks/new-compound")                                  sink-create/compound-attrs]
   [("/sinks/new-compound2")                                 sink-create/compound-attrs2]
   [("/sinks/new-compound3")                                 sink-create/compound-attrs3]
   [("/sinks/new-related")                                   sink-create/related-attrs])
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
