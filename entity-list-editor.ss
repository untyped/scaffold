#lang scheme/base

(require "base.ss")

(require "entity-editor.ss"
         "editor-internal.ss")

(define-mixin entity-list-editor-mixin (html-element<%> editor<%>) ()
  
  (inherit core-html-attributes
           get-editors
           set-editors!)
  
  ; Fields -------------------------------------
  
  ; (cell (listof entity-editor%))
  (cell to-delete null #:accessor #:mutator)
  
  ; (cell (snooze-struct -> entity-editor<%>))
  (init-field editor-maker
    (lambda (struct)
      (let ([ans (new entity-editor% [entity (snooze-struct-entity struct)])])
        (send ans set-value! struct)
        ans))
    #:accessor)
  
  ; Constructor --------------------------------
  
  (init [classes null])
  (super-new [classes (list* 'smoke-entity-list-editor 'ui-widget classes)])
  
  ; Methods ------------------------------------
  
  ; -> (listof snooze-struct)
  (define/public (get-initial-structs)
    (map (cut send <> get-initial-struct)
         (get-editors)))
  
  ; -> (listof snooze-struct)
  (define/public (get-values)
    (map (cut send <> get-value)
         (get-editors)))
  
  ; -> (listof snooze-struct)
  (define/public (set-values! structs)
    (set-editors! (map editor-maker structs)))
  
  ; seed -> xml
  (define/override (render seed)
    (let ([editors (get-editors)])
      (if (null? editors)
          (xml (div (@ ,(core-html-attributes seed))
                    "No editors"))
          (xml (div (@ ,(core-html-attributes seed))
                    ,@(for/list ([editor (in-list (get-editors))])
                        (send editor render seed)))))))
  
  ; -> (listof check-result)
  (define/override (parse)
    (apply check-problems
           (for/list ([editor (in-list (get-editors))])
             (send editor parse))))
  
  ; -> (listof check-result)
  (define/override (validate)
    (apply check-problems
           (for/list ([editor (in-list (get-editors))])
             (send editor validate))))
  
  ; [any] -> snooze-struct
  (define/override (commit-changes #:restructure [restructure (lambda (x) x)])
    (for/list ([editor (in-list (get-editors))])
      (send editor commit-changes
            #:delete?     #f
            #:restructure restructure))
    (for/list ([editor (in-list (get-to-delete))])
      (send editor commit-changes
            #:delete?     #t
            #:restructure restructure))))

; Classes ----------------------------------------

(define entity-list-editor%
  (entity-list-editor-mixin simple-editor%))

; Provide statements -----------------------------

(provide entity-list-editor-mixin
         entity-list-editor%)
