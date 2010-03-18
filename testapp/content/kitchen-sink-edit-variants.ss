#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

; == [1] Default scaffolded page ==
(define-object sink-update-page/vanilla (entity-editor-page-mixin html-page%) ()
  (super-new [title  "A vanilla entity-editor"]
             [entity kitchen-sink]))

; == [2] Default scaffolded page with a subset of attributes ==

(define-object sink-update-page/attrs (entity-editor-page-mixin html-page%) ()
  (super-new [title      "An entity-editor with custom attributes"]
             [entity     kitchen-sink]
             [attributes (attr-list kitchen-sink a-boolean a-real a-integer)]))

; == [3] Single attribute editor modified; rest unchanged ==

(attribute-editor-defaults
 (let ([original (attribute-editor-defaults)])
   (lambda (attribute)
     (if (equal? attribute (attr kitchen-sink a-real))
         (new number-editor% [label "A real, customized"] [attributes (attr-list kitchen-sink a-real)])
         (original attribute)))))

(define-object sink-update-page/customized-attrs (entity-editor-page-mixin html-page%) ()
  (super-new [title "An entity-editor with customized attributes"]
             [entity     kitchen-sink]
             [attributes (attr-list kitchen-sink a-boolean a-real a-integer)]))


; == [4] Multiple-attribute editor, rest unchanged ==

(define-class integer+string-editor% simple-attribute-editor% ()
  ; editor% ...
  (field integer-editor (default-attribute-editor (attr kitchen-sink a-integer)) #:child)
  (field string-editor  (default-attribute-editor (attr kitchen-sink a-string))   #:child)
  
  (super-new [label      "Integer+String"]
             [attributes (attr-list kitchen-sink a-integer a-string)])
  
  ; seed -> xml
  (define/augment (render seed)
    (xml "Str " ,(send string-editor render seed) " >> Integer " ,(send integer-editor render seed)))
  
  ; kitchen-sink -> void
  (define/override (destructure! sink)
    (send string-editor  destructure! sink)
    (send integer-editor destructure! sink))
  
  ; kitchen-sink -> kitchen-sink
  (define/override (restructure sink)
    (send integer-editor restructure (send string-editor restructure sink))))

(define-object sink-editor/compound-attrs entity-editor% ()
  (inherit render-editor render-attributes)
  
  (field a-integer+a-string-editor
    (new integer+string-editor%)
    #:child)
  
  (super-new [entity     kitchen-sink]
             [attributes (attr-list kitchen-sink a-boolean a-real a-integer a-string a-symbol)])
  
  ; seed snooze-struct -> xml
  (define/override (render-editors seed editors)
    (xml ,(render-attributes seed (attr-list kitchen-sink a-boolean a-real))
         ,(render-editor seed a-integer+a-string-editor)
         ,(render-attributes seed (attr-list kitchen-sink a-symbol))))
  
  (define/override (set-value! sink)
    (super set-value! sink)
    (send a-integer+a-string-editor destructure! sink))
  
  (define/override (get-value)
    (send a-integer+a-string-editor restructure (super get-value))))

(define-object sink-update-page/compound-attrs (entity-editor-page-mixin html-page%) ()
  (super-new [title  "An entity-editor with compound attributes"]
             [editor sink-editor/compound-attrs]))


; == [4ii] Multiple-attribute editor, rest unchanged ==

(define-object sink-editor/compound-attrs2 entity-editor% ()
  (super-new [entity     kitchen-sink]
             [attribute-editors `(,@(map default-attribute-editor (attr-list kitchen-sink a-boolean a-real))
                                  ,(new integer+string-editor%)
                                  ,(default-attribute-editor (attr kitchen-sink a-symbol)))]))

(define-object sink-update-page/compound-attrs2 (entity-editor-page-mixin html-page%) ()
  (super-new [title  "An entity-editor with compound attributes2 - the short way"]
             [editor sink-editor/compound-attrs2]))

; == [4iii] Multiple-attribute editor, rest unchanged ==
(define-object sink-update-page/compound-attrs3 (entity-editor-page-mixin html-page%) ()
  (super-new [title  "An entity-editor with compound attributes3 - the really short way"]
             [entity     kitchen-sink]
             [attribute-editors `(,@(map default-attribute-editor (attr-list kitchen-sink a-boolean a-real))
                                  ,(new integer+string-editor%)
                                  ,(default-attribute-editor (attr kitchen-sink a-symbol)))]))

; [5] == View extended with a relationship (i.e. a non-attribute editor) ==

(define-object sink-editor/related-attrs entity-editor% ()
  (inherit get-value render-wrapper render-attributes render-editor)
  (super-new [entity kitchen-sink])
  
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper
       seed
       (xml ,(render-attributes seed struct (attr-list kitchen-sink a-boolean a-real))
            ;,(render-label+value seed "All posts" (render-related-structs seed (find-posts)))
            ,(render-attributes seed struct (attr-list kitchen-sink a-integer a-string a-symbol)))))))

(define-object sink-update-page/related-attrs (entity-editor-page-mixin html-page%) ()
  (super-new [title  "An entity-editor with relations"]
             [editor sink-editor/related-attrs]))

; Update controllers -----------------------------

(define-controller (sink-update/vanilla sink)
  (sink-review/vanilla
   (send* sink-update-page/vanilla
     [set-value! sink]
     [respond])))

(define-controller (sink-update/attrs sink)
  (sink-review/attrs
   (send* sink-update-page/attrs 
     [set-value! sink]
     [respond])))

(define-controller (sink-update/customized-attrs sink)  
  (sink-review/customized-attrs
   (send* sink-update-page/customized-attrs
     [set-value! sink]
     [respond])))

(define-controller (sink-update/compound-attrs sink)
  (sink-review/compound-attrs
   (send* sink-update-page/compound-attrs
     [set-value! sink]
     [respond])))

(define-controller (sink-update/compound-attrs2 sink)
  (sink-review/compound-attrs
   (send* sink-update-page/compound-attrs2
     [set-value! sink]
     [respond])))

(define-controller (sink-update/compound-attrs3 sink)
  (sink-review/compound-attrs
   (send* sink-update-page/compound-attrs3
     [set-value! sink]
     [respond])))

(define-controller (sink-update/related-attrs sink)
  (sink-review/related-attrs
   (send* sink-update-page/related-attrs
     [set-value! sink]
     [respond])))

; Create controllers -----------------------------

(define-controller (sink-create/vanilla)
  (sink-review/vanilla
   (send* sink-update-page/vanilla
     [set-value! (make-kitchen-sink/defaults)]
     [respond])))

(define-controller (sink-create/attrs)
  (sink-review/attrs
   (send* sink-update-page/attrs 
     [set-value! (make-kitchen-sink/defaults)]
     [respond])))

(define-controller (sink-create/customized-attrs)  
  (sink-review/customized-attrs
   (send* sink-update-page/customized-attrs
     [set-value! (make-kitchen-sink/defaults)]
     [respond])))

(define-controller (sink-create/compound-attrs)
  (sink-review/compound-attrs
   (send* sink-update-page/compound-attrs
     [set-value! (make-kitchen-sink/defaults)]
     [respond])))

(define-controller (sink-create/compound-attrs2)
  (sink-review/compound-attrs
   (send* sink-update-page/compound-attrs2
     [set-value! (make-kitchen-sink/defaults)]
     [respond])))

(define-controller (sink-create/compound-attrs3)
  (sink-review/compound-attrs
   (send* sink-update-page/compound-attrs3
     [set-value! (make-kitchen-sink/defaults)]
     [respond])))

(define-controller (sink-create/related-attrs)
  (sink-review/related-attrs
   (send* sink-update-page/related-attrs
     [set-value! (make-kitchen-sink/defaults)]
     [respond])))
