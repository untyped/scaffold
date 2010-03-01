#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol))

; Pages ------------------------------------------

(define editor-page
  (singleton/cells (entity-editor-page-mixin html-page%) ()
    (super-new [entity kitchen-sink])))

; Controllers ------------------------------------

(define-controller (editor)
  (with-connection
   (let ([entity (send editor-page get-entity)])
     (let loop ([val ((entity-defaults-constructor entity))])
       (send editor-page set-value! val)
       (loop (send editor-page respond))))))
