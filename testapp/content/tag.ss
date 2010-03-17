#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3))

; Pages ------------------------------------------

(define-object tag-view entity-view% ()
  (inherit get-attributes get-value render-wrapper render-attributes render-label+value)
  (super-new [entity tag])
  
  ; seed -> xml
  (define/override (render seed)
    (let ([struct (get-value)])
      (render-wrapper
       seed
       (xml ,(render-attributes seed struct (get-attributes))
            ,(render-label+value seed "All posts" 
                                 (render-related-structs seed 
                                                         (select-all #:what  post
                                                                     #:from  (inner post tagging (= post.guid tagging.post))
                                                                     #:where (= tagging.tag ,struct)
                                                                     #:order ((asc post.subject))))))))))

(define-object tag-review-page (entity-review-page-mixin html-page%) ()
  (super-new [view tag-view]))

(define-class tagging-editor% relationship-selector-editor% ()
  (super-new [entity              tag]
             [related-entity      post]
             [relationship-entity tagging])
  
  ; tag post -> tagging
  (define/override (make-relationship a-tag a-post)
    (make-tagging/defaults #:tag a-tag #:post a-post))
  
  ; -> (listof post)
  (define/override (find-relateables)
    (find-posts))
  
  ; Procedures for finding related-/relationship-structs
  ; tag -> (listof tagging)
  (define/override (find-relationships/struct a-tag)
    (find-taggings #:tag a-tag))
  
  ; struct (listof related-structs) -> (listof relationship-struct)
  (define/override (find-relationships/relateds a-tag posts)
    (find-taggings #:post posts))
  
  ; Procedures for extracting the struct and related-struct from a relationship
  ; relationship-struct -> related-struct
  (define/override (relationship->related tgd)
    (tagging-post tgd)))


(define-object tag-editor entity-editor% ()
  (inherit get-value render-label+editor)
  
  (field tagging-editor (new tagging-editor%) #:child)
  
  (super-new [entity tag])
  
  ; seed -> xml
  (define/override (render-editors seed editors)
    (xml ,(super render-editors seed editors)
         ,(render-label+editor seed "Posts" (send tagging-editor render seed))))
  
  ; tag -> void
  (define/override (set-value! tag)
    (super set-value! tag)
    (send tagging-editor set-struct! tag))
  
  ; -> tag
  (define/override (commit-changes)
    (let ([val (get-value)])
      (with-transaction 
          #:metadata (list (if (snooze-struct-saved? val)
                               (format "Created ~a" (format-snooze-struct val))
                               (format "Updated ~a" (format-snooze-struct val))))
        (let ([val (save! val)])
          (send tagging-editor commit-changes/struct! val)
          (clear-continuation-table!)
          val))))
  
  ; These should be eradicated -------------------
  
  ; (listof check-result) -> void
  (define/override (set-check-results! results)
    (super set-check-results! results)
    (send tagging-editor set-check-results! results))
  
  ; -> boolean
  (define/override (value-changed?)
    (or (super value-changed?)
        (send tagging-editor value-changed?)))
  
  ; -> (listof check-result)
  (define/override (parse)
    (check-problems (super parse)
                    (send tagging-editor parse)))
  
  ; -> (listof check-result)
  (define/override (validate)
    (check-problems (super validate)
                    (send tagging-editor validate))))

(define-object tag-editor-page (entity-editor-page-mixin html-page%) ()
  (super-new [entity tag]
             [editor tag-editor]))

; Controllers ------------------------------------

(define-report-controller tag-report tag)
(define-review-controller tag-review tag #:page tag-review-page)
(define-create-controller tag-create tag #:page tag-editor-page)
(define-update-controller tag-update tag #:page tag-editor-page)
(define-delete-controller tag-delete tag #:page tag-review-page)

