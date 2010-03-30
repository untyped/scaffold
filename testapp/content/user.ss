#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

(define-object user-view entity-view% ()
  (inherit get-auto-attributes get-value render-wrapper render-attributes render-label+value)
  (super-new [entity user])
  
  ; seed -> xml
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper
       seed
       (xml ,(render-attributes seed struct (get-auto-attributes))
            ,(render-label+value seed "All posts"    (render-related-structs seed (find-posts    #:user struct)))
            ,(render-label+value seed "All comments" (render-related-structs seed (find-comments #:user struct))))))))

(define-object user-review-page (entity-review-page-mixin html-page%) ()
  (super-new [view user-view]))

; Controllers ------------------------------------

(define-report-controller user-report user)
(define-review-controller user-review user #:page user-review-page)
(define-create-controller user-create user)
(define-update-controller user-update user)
(define-delete-controller user-delete user)