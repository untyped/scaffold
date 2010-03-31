#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

; == [1] Default scaffolded page ==
(define-object sink-update-page/vanilla (entity-editor-page-mixin html-page%) ()
  (super-new [title  "A vanilla entity-editor"]
             [entity kitchen-sink]))

; == [2] Default scaffolded page with a subset of h ==

(define-object sink-update-page/attrs (entity-editor-page-mixin html-page%) ()
  (super-new [title      "An entity-editor with custom attributes"]
             [entity     kitchen-sink]
             [auto-attributes (attr-list kitchen-sink a-boolean a-real a-integer)]))

; == [3] Single attribute editor modified; rest unchanged ==

; attribute -> xml+quotable
(attribute-label-xml-defaults
 (let ([original (attribute-label-xml-defaults)])
   (lambda (attribute)
     (if (eq? attribute (attr kitchen-sink a-real))
         "A real, customized"
         (original attribute)))))

(define-object sink-update-page/customized-attrs (entity-editor-page-mixin html-page%) ()
  (super-new [title           "An entity-editor with customized attributes"]
             [entity          kitchen-sink]
             [auto-attributes (attr-list kitchen-sink a-boolean a-real a-integer)]))


; == [4] Multiple-attribute editor, rest unchanged ==

(define-class integer+string-editor% compound-editor% ()
  
  ; Fields ---------------------------------------
  ; editor% ...
  (field integer-editor (default-attribute-editor (attr kitchen-sink a-integer)) #:child)
  (field string-editor  (default-attribute-editor (attr kitchen-sink a-string))  #:child)
  
  ; Constructor ----------------------------------
  
  (super-new [sub-editors (list integer-editor string-editor)])
  
  ; Methods --------------------------------------
  
  ; seed -> xml
  (define/override (render seed)
    (xml "Str " ,(send string-editor render seed) " >> Integer " ,(send integer-editor render seed)))
  
  ; (listof integer string) -> void
  (define/public (set-value! int+str)
    (match-let ([(list int str) int+str])
      (send integer-editor set-value! int)
      (send string-editor  set-value! str)))
  
  ; -> (listof integer string)
  (define/public (get-value)
    (list (send integer-editor get-value)
          (send string-editor  get-value))))

(define-object sink-editor/compound-attrs entity-editor% ()
  (inherit render-label+editor)
  
  (field a-integer+a-string-editor
    (new integer+string-editor%)
    #:child)
  
  (super-new [entity          kitchen-sink]
             [auto-attributes (attr-list kitchen-sink a-boolean a-real a-symbol)])
  
  ; seed snooze-struct -> xml
  (define/override (render-attributes seed attrs)
    (xml ,(super render-attributes seed (attr-list kitchen-sink a-boolean a-real))
         ,(render-label+editor seed "Integer+String" a-integer+a-string-editor)
         ,(super render-attributes seed (attr-list kitchen-sink a-symbol))))
  
  ; kitchen-sink -> void
  (define/override (set-value! sink)
    (super set-value! sink)
    (send a-integer+a-string-editor set-value! (list (kitchen-sink-a-string sink) (kitchen-sink-a-integer sink))))
  
  ; -> kitchen-sink
  (define/override (get-value)
    (match-let ([(list an-int a-str) (send a-integer+a-string-editor get-value)])
      (kitchen-sink-set (super get-value) #:a-integer an-int #:a-string a-str))))

(define-object sink-update-page/compound-attrs (entity-editor-page-mixin html-page%) ()
  (super-new [title  "An entity-editor with compound attributes"]
             [editor sink-editor/compound-attrs]))

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