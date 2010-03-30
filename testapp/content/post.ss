#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

(define-object post-view entity-view% ()
  (inherit get-auto-attributes get-value render-wrapper render-attributes render-label+value)
  (super-new [entity post])
  
  ; seed -> xml
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper
       seed
       (xml ,(render-attributes seed struct (get-auto-attributes))
            ,(render-label+value seed "All comments" (render-related-structs seed (find-comments #:post struct))))))))

(define-object post-review-page (entity-review-page-mixin html-page%) ()
  (super-new [view post-view]))

; Controllers ------------------------------------

(define-report-controller post-report post)
(define-review-controller post-review post #:page post-review-page)
(define-create-controller post-create post)
(define-update-controller post-update post)
(define-delete-controller post-delete post)

