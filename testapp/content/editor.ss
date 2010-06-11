#lang scheme/base

(require "../content-base.ss")

(require (snooze-in)
         (unlib-in list symbol))

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
