#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

(define-object comment-editor-page (entity-editor-page-mixin html-page%) ()
  (super-new [entity comment]))

(define-object comment-review-page (entity-review-page-mixin html-page%) ()
  (super-new [entity comment]))

; Controllers ------------------------------------

(define-controller (comment-creator)
  (let loop ([val ((entity-defaults-constructor comment))])
    (send comment-editor-page set-value! val)
    (loop (send comment-editor-page respond))))

(define-controller (comment-review comm)
  (send* comment-review-page
    [set-value! comm]
    [respond]))

(define-controller (comment-editor comm)
  (let loop ([val comm])
    (send comment-editor-page set-value! val)
    (loop (send comment-editor-page respond))))
