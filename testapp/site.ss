#lang scheme/base

(require "../base.ss")

(require (dispatch-in)
         "../arg.ss"
         "model/db.ss"
         "model/entities.ss")

; Site -------------------------------------------

(define-site test-site
  ([("")                                                 home]
   ; post pages
   [("/posts")                                            post-report]
   [("/posts/new")                                        post-creator]
   [("/posts/edit/"            (entity-arg post))         post-editor]
   [("/posts/view/"            (entity-arg post))         post-review]
   ; comment pages
   [("/comments")                                         comment-report]
   [("/comments/new")                                     comment-creator]
   [("/comments/edit/"         (entity-arg comment))      comment-editor]
   [("/comments/view/"         (entity-arg comment))      comment-review]
   ; kitchen sink pages
   [("/sinks")                                            sink-report]
   [("/sinks/new")                                        sink-creator]
   [("/sinks/edit/"            (entity-arg kitchen-sink)) sink-editor]
   [("/sinks/view/"            (entity-arg kitchen-sink)) sink-review]
   [("/sinks/view-attrs/"      (entity-arg kitchen-sink)) sink-review/attrs]
   [("/sinks/view-customized/" (entity-arg kitchen-sink)) sink-review/customized-attrs]
   [("/sinks/view-compound/"   (entity-arg kitchen-sink)) sink-review/compound-attrs]
   [("/sinks/view-related/"    (entity-arg kitchen-sink)) sink-review/related-attrs])
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
