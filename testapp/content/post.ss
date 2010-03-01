#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

(define-object post-editor-page (entity-editor-page-mixin html-page%) ()
  (super-new [entity post]))

(define-object post-view entity-view% ()
  (inherit get-attributes get-value render-wrapper render-attributes render-label+value)
  (super-new [entity post])
  
  ; seed -> xml
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper
       seed
       (xml ,(render-attributes seed struct (get-attributes))
            ,(render-label+value seed "All comments" (render-related-structs seed (find-comments #:post struct))))))))

(define-object post-review-page (entity-review-page-mixin html-page%) ()
  (super-new [view post-view]))

; Controllers ------------------------------------

(define-controller (post-review post)
  (send* post-review-page
    [set-value! post]
    [respond]))

(define-controller (post-creator)
  (let loop ([val ((entity-defaults-constructor post))])
    (send post-editor-page set-value! val)
    (loop (send post-editor-page respond))))

(define-controller (post-editor post)
  (let loop ([val post])
    (send post-editor-page set-value! val)
    (loop (send post-editor-page respond))))

