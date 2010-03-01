#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "check-label.ss"
         "controller-internal.ss")

; Procedures -------------------------------------

; (parameter (attribute any -> xml))
(define attribute-xml-defaults
  (make-parameter
   (lambda (attr val)
     val)))

; attribute any -> xml
(define (attribute-xml attr val)
  ((attribute-xml-defaults) attr val))

; (parameter (snooze-struct attribute -> xml))
(define snooze-struct-xml-ref-defaults
  (make-parameter
   (lambda (struct attr)
     (let ([type (attribute-type attr)]
           [val  (snooze-struct-ref struct attr)])
       (cond [(snooze-struct? val) (format-snooze-struct val)]
             [(guid? val)          (format-snooze-struct (find-by-guid val))]
             [(and (enum-type? type) (enum-type-enum type))
              => (cut enum-prettify <> val)]
             [else val])))))

; snooze-struct attribute -> xml
(define (snooze-struct-xml-ref struct attr)
  ((snooze-struct-xml-ref-defaults) struct attr))

; Interfaces -------------------------------------

(define entity-view<%>
  (interface ()
    get-entity     ; -> entity
    get-attributes ; -> (listof attribute)
    get-value      ; -> any
    set-value!))   ; any -> void

; Mixins -----------------------------------------

(define entity-view-mixin
  (mixin/cells (html-element<%>) (entity-view<%>)
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof attribute)
    (init-field attributes (and entity (entity-data-attributes entity)) #:accessor)
    
    ; (cellof (U snooze-struct #f))
    (init-cell value #f #:accessor #:mutator)
    
    ; (listof (U string symbol))
    (init [classes null])
    
    (super-new [classes (list* 'entity-view 'ui-widget classes)])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (let ([struct (get-value)])
        (render-wrapper seed (render-attributes seed struct (get-attributes)))))
    
    ; seed xml+quotable -> xml
    (define/public (render-wrapper seed contents)
      (xml (table (@ ,(core-html-attributes seed))
                  (tbody ,contents))))
    
    ; seed snooze-struct (listof attribute) -> xml
    (define/public (render-attributes seed struct attributes)
      (xml ,@(for/list ([attribute (in-list attributes)])
               (render-attribute seed struct attribute))))
    
    ; seed snooze-struct attribute -> xml
    (define/public (render-attribute seed struct attribute)
      (render-label+value seed
                          (attribute-pretty-name attribute)
                          (snooze-struct-xml-ref struct attribute)))
    
    ; seed xml+quotable xml+quotable -> xml
    (define/public (render-label+value seed label value)
      (xml (tr (th ,label)
               (td ,value))))))


(define entity-view%
  (entity-view-mixin html-element%))

; Helpers ----------------------------------------

; seed (listof snooze-struct) -> xml
(define (render-related-structs seed relateds)
  (xml (ul (@ [class "relationship-view"])
           ,@(for/list ([related (in-list relateds)])
               (xml (li ,(cond [(review-controller-set? related)
                                (controller-link seed (review-controller-ref related) related
                                                 #:body (format-snooze-struct related))]
                               [else (format-snooze-struct related)])))))))

; Provide statements -----------------------------

(provide/contract 
 [attribute-xml-defaults         (parameter/c (-> attribute? any/c xml+quotable?))]
 [attribute-xml                  (-> attribute? any/c xml+quotable?)]
 [snooze-struct-xml-ref-defaults (parameter/c (-> snooze-struct? attribute? xml+quotable?))]
 [snooze-struct-xml-ref          (-> snooze-struct? attribute? xml+quotable?)]
 [render-related-structs         (-> seed? (listof snooze-struct?) xml?)])

(provide entity-view<%>
         entity-view-mixin
         entity-view%)
