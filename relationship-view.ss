#lang scheme/base

(require "base.ss")

(require "attribute-view.ss"
         "controller-internal.ss"
         "util.ss")

; Components -------------------------------------

; entity represents the independent entity: there is only one struct of this entity type.
; related-entity represents the type of the related structs, of which there may be zero to many.
; relationship-entity represents a type that represents the relationship between entity and related-entity.
(define relationship-view%
  (class/cells attribute-view% () 
    (inherit core-html-attributes get-value set-value!)
    
    ; entity field represents the independent entity (only 1 struct of this type)
    ; entity ...
    (init-field entity              #f #:accessor)
    (init-field related-entity      #f #:accessor) ; the dependent entity (n structs of this type)
    (init-field relationship-entity #f #:accessor) ; the entity that encapsulates the relationship (may be same as related)
    
    ; Constructor --------------------------------
    
    ; (listof (U string symbol))
    (init [classes null])
    
    (super-new [classes (list* "relationship-view" classes)])
    
    ; Methods ------------------------------------
    
    ; snooze-struct -> void
    (define/override (destructure! struct)
      (set-value! (map (cut relationship->related <>) (find-relationships/struct struct))))
    
    ; interface methods --------------------------
    
    ; Procedures for finding related-/relationship-structs
    ; struct -> (listof relationship-struct)
    (define/public (find-relationships/struct struct)
      (error "find-relationships/struct must be overridden"))
    
    ; Procedures for extracting the struct and related-struct from a relationship
    ; relationship-struct -> related-struct
    (define/public (relationship->related relationship)
      (error "relationship->related must be overridden"))
    
    ; Rendering ----------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (ul (@ ,(core-html-attributes seed))
               ,@(for/list ([related (in-list (get-value))])
                   (xml (li ,(render-related seed related)))))))
    
    ; seed related-struct -> xml
    (define/public (render-related seed related)
      (cond [(review-controller-set? related)
             (xml (a (@ [href ,(review-controller-url related)])
                     ,(format-snooze-struct related)))]
            [else (format-snooze-struct related)]))))

; Provides ---------------------------------------

(provide relationship-view%)
