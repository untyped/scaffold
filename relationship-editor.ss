#lang scheme/base

(require "base.ss")

(require (smoke-in core/send)
         "attribute-editor-internal.ss"
         "foreign-key-editor.ss"
         "util.ss")

; Components -------------------------------------

(define relationship-combo-box-editor% 
  (class/cells foreign-key-editor% ()
    (inherit get-entity)
    ; (U snooze-struct #f) -> string
    (define/override (option->string option)
      (if option
          (format-snooze-struct option)
          (format "-- Select ~a --" (entity-pretty-name (get-entity)))))))

; entity represents the independent entity: there is only one struct of this entity type.
; related-entity represents the type of the related structs, of which there may be zero to many.
; relationship-entity represents a type that represents the relationship between entity and related-entity.
(define relationship-selector-editor%
  (class/cells (complete-attribute-editor-mixin vanilla-set-selector%) () 
    (inherit get-editor get-value set-value! activate-item)
    
    ; entity field represents the independent entity (only 1 struct of this type)
    ; entity ...
    (init-field entity          #f #:accessor)
    (init-field related-entity  #f #:accessor) ; the dependent entity (n structs of this type)
    (init-field relationship-entity #f #:accessor) ; the entity that encapsulates the relationship (may be same as related)
    
    ; SQL details --------------------------------
    
    ; (U sql-expr #f)
    (init [where #f])
    
    ; (cell (listof sql-order))
    (init-cell order
      (let-sql ([entity (get-related-entity)])
        (sql-list (asc entity.guid)))
      #:accessor #:mutator)
    
    ; Cells --------------------------------------
    
    ; (U snooze-struct #f)
    (init-cell struct #f #:accessor #:mutator)
    
    ; (listof relationship-struct) ...
    (init-cell updated-relationships null #:accessor #:mutator)
    (init-cell deleted-relationships null #:accessor #:mutator) 
    
    ; (listof snooze-struct)
    (init-cell available-items null #:override-accessor #:mutator)
    
    ; Fields -------------------------------------        
    
    ; combo-box%
    (super-new [editor (new relationship-combo-box-editor% 
                            [entity    related-entity]
                            [classes   (list "editor")]
                            [on-change (callback activate-item)]
                            [where     where]
                            [order     order])])
    
    ; Methods ------------------------------------
    
    ; -> (U number symbol)
    (define/override (get-editor-value) 
      (let ([editor-value (send (get-editor) get-value)])
        (and editor-value (snooze-struct-id editor-value))))
    
    ; -> void
    (define/override (reset-editor-value) 
      (send (get-editor) set-value! #f))
    
    ; any -> (U boolean integer symbol)
    (define/override (item->raw item)
      (and item
           (if (and (snooze-struct?       item)
                    (snooze-struct-saved? item))
               (number->string (snooze-struct-id item))
               (raise-type-error 'foreign-key-editor.item->raw
                                 "(U snooze-struct #f)"
                                 item))))
    
    ; (U string snooze-struct) -> any
    (define/override (raw->item raw)
      (and raw
           ; for some reason, select-item passes in an integer, while deselect-item prefers a string.
           ; FIXME, later...
           (cond [(number? raw) (find-by-id (get-related-entity) raw)]
                 [(string? raw) (find-by-id (get-related-entity) (string->number raw))])))
    
    ; any -> string
    (define/override (item->string item)
      (if item (format-snooze-struct item) "None"))
    
    ; -> boolean
    (define/override (items-available?)
      (pair? (get-available-items)))
    
    ; -> void
    (define/override (refresh-selectable-items)
      (let-sql ([entity (get-related-entity)])
        (send (get-editor) set-where! (sql (and (in entity.guid ,(find-relateables))
                                                (not (in entity.guid ,(get-value))))))))
    
    ; -> sql-where
    (define/public (get-where)
      (send (get-editor) get-where))
    
    ; sql-where -> void
    (define/public (set-where! where-clause)
      (send (get-editor) set-where! where-clause))
    
    ; Methods ------------------------------------
    
    ; snooze-struct -> void
    (define/override (destructure! struct)
      (set-struct! struct)
      (set-available-items! (find-relateables))
      (set-value! (map (cut relationship->related <>) (find-relationships/struct struct))))
    
    ; snooze-struct -> snooze-struct
    (define/override (restructure struct)
      struct)
    
    ; interface methods --------------------------
    
    ; A procedure for making a new entity of the type: relationship-entity
    ; struct related-struct -> relationship-struct
    (define/public (make-relationship struct related)
      (error "make-relationship must be overridden"))
    
    ; A procedure for finding all related-structs that /may/ be involved in the relationship
    ; -> (listof related-struct)
    (define/public (find-relateables)
      (error "find-relateables must be overridden"))
    
    ; Procedures for finding related-/relationship-structs
    ; struct -> (listof relationship-struct)
    (define/public (find-relationships/struct struct)
      (error "find-relationships/struct must be overridden"))
    
    ; struct (listof related-structs) -> (listof relationship-struct)
    (define/public (find-relationships/relateds struct relateds)
      (error "find-relationships/relateds must be overridden"))
    
    ; Procedures for extracting the struct and related-struct from a relationship
    ; relationship-struct -> related-struct
    (define/public (relationship->related relationship)
      (error "relationship->related must be overridden"))
    
    (define/private (update-cells)
      (let* ([struct                 (get-struct)]
             ; related-structs currently selected in the set-selector
             [chosen-related-structs (get-value)]
             ; relationship-structs that are currently saved
             [saved-relationship-structs (find-relationships/relateds struct chosen-related-structs)]
             ; a list of relationship-structs that are either saved (from above) or newly created
             [saved+new-relationships    
              (reverse 
               (for/fold ([relationships null])
                         ([chosen-related (in-list chosen-related-structs)])
                         (cons (or (findf (lambda (relationship)
                                            (= (snooze-struct-id (relationship->related relationship))
                                               (snooze-struct-id chosen-related)))
                                          saved-relationship-structs)
                                   (make-relationship struct chosen-related))
                               relationships)))]
             [deleted-relationships      (filter (lambda (rel) 
                                                   (not (member (snooze-struct-id rel)
                                                                (map snooze-struct-id saved+new-relationships))))
                                                 (find-relationships/struct struct))])
        (set-updated-relationships! saved+new-relationships)
        (for ([rel (in-list saved+new-relationships)])
          (for ([attr (in-list (entity-data-attributes (get-relationship-entity)))])
            (snooze-struct-ref rel attr)))
        (set-deleted-relationships! deleted-relationships)))
    
    ; Nothing to parse - no user entry, only choice of preselected options
    ; -> (listof check-result)
    (define/override (parse)
      null)
    
    ; -> (listof check-result)
    (define/override (validate)
      (update-cells)
      (check/annotate ([ann:form-elements (list this)])
        (check-problems (apply check-problems (map check-snooze-struct     (get-updated-relationships)))
                        (apply check-problems (map check-old-snooze-struct (get-deleted-relationships))))))
    
    ; TODO at present this must be overridden to copy the newly saved (get-struct) into each updated relationship
    (define/public (commit-changes)
      (call-with-transaction 
       #:metadata (list "Saving relationships for ~a" (get-struct))
       (lambda ()
         (begin0 (map save!   (get-updated-relationships))
                 (map delete! (get-deleted-relationships))
                 (clear-continuation-table!)))))))

; Provides ---------------------------------------

(provide relationship-selector-editor%)
