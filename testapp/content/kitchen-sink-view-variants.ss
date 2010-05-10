#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

; == [1] Default scaffolded page ==
(define-object sink-review-page/vanilla (entity-review-page-mixin html-page%) ()
  (super-new [title  "A vanilla entity-view"]
             [entity kitchen-sink]))

; == [2] Default scaffolded page with a subset of attributes ==

(define-object sink-review-page/attrs (entity-review-page-mixin html-page%) ()
  (super-new [title      "An entity-view with custom attributes"]
             [entity     kitchen-sink]
             [auto-attributes (attr-list kitchen-sink a-boolean a-real a-integer)]))

; == [3] Single attribute view modified; rest unchanged ==

(define-object sink-view/attrs-customized entity-view% ()
  (inherit render-label+value)
  (super-new [entity          kitchen-sink]
             [auto-attributes (attr-list kitchen-sink a-boolean a-real a-integer)])
  (define/override (render-attribute seed struct attribute)
    (if (eq? attribute (attr kitchen-sink a-real))
        (render-label+value seed "A real, customized" (xml (b ,(snooze-struct-xml-ref struct attribute))))
        (super render-attribute seed struct attribute))))

(define-object sink-review-page/customized-attrs (entity-review-page-mixin html-page%) ()
  (super-new [title "An entity-view with customized attributes"]
             [view  sink-view/attrs-customized]))


; == [4] Compound view, rest unchanged ==

(define-object sink-view/compound-attrs entity-view% ()
  (inherit render-label+value)
  (super-new [entity     kitchen-sink]
             [auto-attributes (attr-list kitchen-sink a-boolean a-real a-integer a-string a-symbol)])
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

(define-object sink-review-page/compound-attrs (entity-review-page-mixin html-page%) ()
  (super-new [title "An entity-view with compound attributes"]
             [view  sink-view/compound-attrs]))

; [5] == View extended with a relationship (i.e. a non-attribute view) ==

(define-object sink-view/related-attrs entity-view% ()
  (inherit get-value render-wrapper render-attributes render-label+value)
  (super-new [entity kitchen-sink])
  
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper
       seed
       (xml ,(render-attributes seed struct (attr-list kitchen-sink a-boolean a-real))
            ,(render-label+value seed "All posts" (render-related-structs seed (find-posts)))
            ,(render-attributes seed struct (attr-list kitchen-sink a-integer a-string a-symbol)))))))

(define-object sink-review-page/related-attrs (entity-review-page-mixin html-page%) ()
  (super-new [title "An entity-view with relations"]
             [view  sink-view/related-attrs]))

; Controllers ------------------------------------

; Custom controllers
(define-controller (sink-review/vanilla sink)
  (send* sink-review-page/vanilla
    [set-value! sink]
    [respond]))

; Custom controllers
(define-controller (sink-review/attrs sink)
  (send* sink-review-page/attrs 
    [set-value! sink]
    [respond]))

(define-controller (sink-review/customized-attrs sink)
  (send* sink-review-page/customized-attrs
    [set-value! sink]
    [respond]))

(define-controller (sink-review/compound-attrs sink)
  (send* sink-review-page/compound-attrs
    [set-value! sink]
    [respond]))

(define-controller (sink-review/related-attrs sink)
  (send* sink-review-page/related-attrs
    [set-value! sink]
    [respond]))
