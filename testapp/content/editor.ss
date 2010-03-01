#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol))

; Pages ------------------------------------------

(define kitchen-sink-editor-page
  (singleton/cells (entity-editor-page-mixin html-page%) ()
    (super-new [entity kitchen-sink])))

(define post-editor-page
  (singleton/cells (entity-editor-page-mixin html-page%) ()
    (super-new [entity post])))

(define comment-editor-page
  (singleton/cells (entity-editor-page-mixin html-page%) ()
    (super-new [entity comment])))

; Controllers ------------------------------------

(define-controller (sink-creator)
  (let ([entity (send kitchen-sink-editor-page get-entity)])
    (let loop ([val ((entity-defaults-constructor entity))])
      (send kitchen-sink-editor-page set-value! val)
      (loop (send kitchen-sink-editor-page respond)))))

(define-controller (sink-editor sink)
  (let loop ([val sink])
    (send kitchen-sink-editor-page set-value! val)
    (loop (send kitchen-sink-editor-page respond))))


(define-controller (post-creator)
  (let loop ([val ((entity-defaults-constructor post))])
    (send post-editor-page set-value! val)
    (loop (send post-editor-page respond))))

(define-controller (post-editor post)
  (let loop ([val post])
    (send post-editor-page set-value! val)
    (loop (send post-editor-page respond))))

(define-controller (comment-creator)
  (let loop ([val ((entity-defaults-constructor comment))])
    (send comment-editor-page set-value! val)
    (loop (send comment-editor-page respond))))

(define-controller (comment-editor comm)
  (let loop ([val comm])
    (send comment-editor-page set-value! val)
    (loop (send comment-editor-page respond))))
