#lang scheme/base

(require "../content-base.ss")

(require (unlib-in list symbol))

; Pages ------------------------------------------

(define review-page
  (singleton/cells (entity-review-page-mixin html-page%) ()
    (super-new [title  "A vanilla entity-view"]
               [entity kitchen-sink])))

(define review/attrs-page
  (singleton/cells (entity-review-page-mixin html-page%) ()
    (super-new [title      "An entity-view with custom attributes"]
               [entity     kitchen-sink]
               [attributes (attr-list kitchen-sink a-boolean a-real a-integer)])))

; == [3] Single attribute view modified; rest unchanged ==

(define-object customized-kitchen-sink entity-view% ()
  (inherit render-label+value)
  (super-new [entity     kitchen-sink]
             [attributes (attr-list kitchen-sink a-boolean a-real a-integer)])
    (define/override (render-attribute seed struct attribute)
      (if (eq? attribute (attr kitchen-sink a-real))
          (render-label+value seed "A real, customized" (xml (b ,(snooze-struct-xml-ref struct attribute))))
          (super render-attribute seed struct attribute))))
  
(define review-customized-attrs-page
  (singleton/cells (entity-review-page-mixin html-page%) ()
    (super-new [title "An entity-view with customized attributes"]
               [view  customized-kitchen-sink])))

; Controllers ------------------------------------

(define-controller (review)
  (with-connection
   (let ([entity (send review-page get-entity)])
     (let loop ([val ((entity-defaults-constructor entity))])
       (send review-page set-value! val)
       (loop (send review-page respond))))))

(define-controller (review/attrs)
  (with-connection
   (let ([entity (send review/attrs-page get-entity)])
     (let loop ([val ((entity-defaults-constructor entity))])
       (send review/attrs-page set-value! val)
       (loop (send review/attrs-page respond))))))

(define-controller (review/customized-attrs)
  (with-connection
   (let ([entity (send review-customized-attrs-page get-entity)])
     (let loop ([val ((entity-defaults-constructor entity))])
       (send review-customized-attrs-page set-value! val)
       (loop (send review-customized-attrs-page respond))))))
