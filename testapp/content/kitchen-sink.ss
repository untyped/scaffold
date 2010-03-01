#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

(define-object kitchen-sink-editor-page (entity-editor-page-mixin html-page%) ()
  (super-new [entity kitchen-sink]))

; == [1] Default scaffolded page ==
(define-object review-page (entity-review-page-mixin html-page%) ()
  (super-new [title  "A vanilla entity-view"]
             [entity kitchen-sink]))

; == [2] Default scaffolded page with a subset of attributes ==

(define-object review/attrs-page (entity-review-page-mixin html-page%) ()
  (super-new [title      "An entity-view with custom attributes"]
             [entity     kitchen-sink]
             [attributes (attr-list kitchen-sink a-boolean a-real a-integer)]))

; == [3] Single attribute view modified; rest unchanged ==

(define-object customized-kitchen-sink entity-view% ()
  (inherit render-label+value)
  (super-new [entity     kitchen-sink]
             [attributes (attr-list kitchen-sink a-boolean a-real a-integer)])
  (define/override (render-attribute seed struct attribute)
    (if (eq? attribute (attr kitchen-sink a-real))
        (render-label+value seed "A real, customized" (xml (b ,(snooze-struct-xml-ref struct attribute))))
        (super render-attribute seed struct attribute))))

(define-object review-customized-attrs-page (entity-review-page-mixin html-page%) ()
  (super-new [title "An entity-view with customized attributes"]
             [view  customized-kitchen-sink]))


; == [4] Multiple-attribute view, rest unchanged ==

(define-object compound-kitchen-sink entity-view% ()
  (inherit render-label+value)
  (super-new [entity     kitchen-sink]
             [attributes (attr-list kitchen-sink a-boolean a-real a-integer a-string a-symbol)])
  ; seed snooze-struct -> xml
  (define/override (render-attributes seed struct attributes)
    (xml ,(super render-attributes seed struct (attr-list kitchen-sink a-boolean a-real))
         ,(render-label+value 
           seed
           "A-integer+a-string" 
           (xml (span ,(snooze-struct-xml-ref struct (attr kitchen-sink a-integer)) 
                      " >> "
                      ,(snooze-struct-xml-ref struct (attr kitchen-sink a-string)))))
         ,(super render-attributes seed struct (attr-list kitchen-sink a-symbol)))))

(define-object review-compound-attrs-page (entity-review-page-mixin html-page%) ()
  (super-new [title "An entity-view with compound attributes"]
             [view  compound-kitchen-sink]))

; [5] == View extended with a relationship (i.e. a non-attribute view) ==

(define-object entity/related-view entity-view% ()
  (inherit get-value render-wrapper render-attributes render-label+value)
  (super-new [entity kitchen-sink])
  
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper
       seed
       (xml ,(render-attributes seed struct (attr-list kitchen-sink a-boolean a-real))
            ,(render-label+value seed "All posts" (render-related-structs seed (find-posts)))
            ,(render-attributes seed struct (attr-list kitchen-sink a-integer a-string a-symbol)))))))

(define-object review-relations-page (entity-review-page-mixin html-page%) ()
  (super-new [title "An entity-view with relations"]
             [view  entity/related-view]))

; Controllers ------------------------------------

(define-controller (sink-review sink)
  (send* review-page 
    [set-value! sink]
    [respond]))

(define-controller (sink-review/attrs sink)
  (send* review/attrs-page 
    [set-value! sink]
    [respond]))

(define-controller (sink-review/customized-attrs sink)
  (send* review-customized-attrs-page
    [set-value! sink]
    [respond]))

(define-controller (sink-review/compound-attrs sink)
  (send* review-compound-attrs-page
    [set-value! sink]
    [respond]))

(define-controller (sink-review/related-attrs sink)
  (send* review-relations-page
    [set-value! sink]
    [respond]))

(define-controller (sink-creator)
  (let loop ([val ((entity-defaults-constructor kitchen-sink))])
    (send kitchen-sink-editor-page set-value! val)
    (loop (send kitchen-sink-editor-page respond))))

(define-controller (sink-editor sink)
  (let loop ([val sink])
    (send kitchen-sink-editor-page set-value! val)
    (loop (send kitchen-sink-editor-page respond))))
