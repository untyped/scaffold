#lang scheme/base

(require "base.ss")

(require (unlib-in string)
         "entity-report.ss"
         "page-internal.ss")

; Variables --------------------------------------

; (parameter snooze-report%)
(define default-scaffolded-report-superclass
  (make-parameter entity-report%))

; Mixins -----------------------------------------

(define-mixin entity-report-page-mixin (html-element<%> html-page<%>) ()
  
  (inherit get-id)
  
  ; Fields ----------------------------
  
  (super-new)
  
  ; entity
  (init [entity #f])
  
  ; (listof attribute)
  (init [auto-attributes (and entity (entity-data-attributes entity))])
  
  ; snooze-report%
  (init-field report
    (or (and entity (new (default-scaffolded-report-superclass) [entity entity] [auto-attributes auto-attributes]))
        (string-append "entity-report-page constructor: insufficient arguments"))
    #:child)
  
  ; Methods ---------------------------
  
  ; -> entity
  (define/public (get-entity)
    (send report get-entity))
  
  ; -> string
  (define/override (get-title)
    (or (super get-title)
        (string-sentencecase (entity-pretty-name-plural (get-entity)))))
  
  ; seed -> xml
  (define/augment (render seed)
    (send report render seed)))

; Procedures -------------------------------------

; entity [(subclassof html-page%)] [#:auto-attributes (listof attribute)] -> html-page%
(define (scaffold-report-page entity 
                              [page% (default-scaffolded-page-superclass)]
                              #:auto-attributes [auto-attributes (entity-data-attributes entity)])
  (new (entity-report-page-mixin page%) [entity entity] [auto-attributes auto-attributes]))

; Provide statements -----------------------------

(provide entity-report-page-mixin)

(provide/contract
 [default-scaffolded-report-superclass (parameter/c (subclass?/c entity-report%))]
 [scaffold-report-page                 (->* (entity?)
                                            ((subclass?/c html-page%)
                                             #:auto-attributes (listof attribute?))
                                            (is-a?/c html-page%))])