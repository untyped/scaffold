#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol))

; Components -------------------------------------

(define stuff-editor% 
  (class/cells relationship-selector-editor% ()
    (inherit get-struct get-updated-relationships get-deleted-relationships)
    
    (super-new [entity              person]
               [related-entity      stuff]
               [relationship-entity owner])
    
    ; A procedure for making a new entity of the type: relationship-entity
    ; struct related-struct -> relationship-struct
    (define/override (make-relationship struct related)
      (make-owner/defaults #:person struct #:stuff related))
    
    ; A procedure for finding all related-structs that /may/ be involved in the relationship
    ; -> (listof related-struct)
    (define/override (find-relateables)
      (find-stuffs))
    
    ; Procedures for finding related-/relationship-structs
    ; struct -> (listof relationship-struct)
    (define/override (find-relationships/struct struct)
      (find-owners #:person struct))
    
    ; struct (listof related-structs) -> (listof relationship-struct)
    (define/override (find-relationships/relateds struct relateds)
      (find-owners #:person struct #:stuff relateds))
    
    ; Procedures for extracting the struct and related-struct from a relationship
    ; relationship-struct -> related-struct
    (define/override (relationship->related relationship)
      (owner-stuff relationship))
    
    (define/override (commit-changes)
      (let ([pers (get-struct)])
        (call-with-transaction 
         #:metadata (list "Saving relationships for ~a" pers)
         (lambda ()
           (begin0 (map (lambda (owner)
                          (save!   (owner-set owner #:person pers)))
                        (get-updated-relationships))
                   (map delete! (get-deleted-relationships))
                   (clear-continuation-table!))))))))

(define person-editor%
  (class/cells entity-editor% ()
    (field stuff-owned-editor 
      (new stuff-editor% [label "Stuff"])
      #:child)
    (super-new [entity person])
    (define/override (get-editors)
      (append (super get-editors) (list stuff-owned-editor)))
    (define/override (validate)
      (check-problems (super validate)
                      (send stuff-owned-editor validate)))
    (define/override (commit-changes)
      (with-transaction "Saving person and stuff"
        (begin0 (super commit-changes)
                (send stuff-owned-editor commit-changes)
                (clear-continuation-table!))))))

; Pages ------------------------------------------

(define stuff-editor-page
  (singleton/cells (entity-editor-page-mixin (render-augride-mixin html-page%)) ()
    (super-new [entity stuff])))

(define person-editor-page
  (singleton/cells (entity-editor-page-mixin (render-augride-mixin html-page%)) ()
    (super-new [entity     person]
               [editor     (new person-editor%)])))

; Controllers ------------------------------------

(define-controller (stuff-editor)
  (with-connection
   (let ([entity (send stuff-editor-page get-entity)])
     (let loop ([val ((entity-defaults-constructor entity))])
       (send stuff-editor-page set-value! val)
       (loop (send stuff-editor-page respond))))))

(define-controller (person-editor)
  (with-connection
   (let ([entity (send person-editor-page get-entity)])
     (let loop ([val ((entity-defaults-constructor entity))])
       (send person-editor-page set-value! val)
       (loop (send person-editor-page respond))))))
