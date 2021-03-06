#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base
                     scheme/match
                     syntax/parse
                     (cce-scheme-in syntax)
                     (snooze-in core/syntax-info)
                     (unlib-in syntax))
         "controller-internal.ss"
         "delete-page.ss"
         "editor-page.ss"
         "report-page.ss"
         "review-page.ss")

; Helpers ----------------------------------------

;  syntax             ; #'(define-report-controller etc ...)
;  syntax             ; #'scaffold-report-page
;  syntax             ; #'report-controller-set!
;  (syntax -> syntax) ; (#'page-id -> #'(send* page-id ...))
(define-for-syntax (make-controller-definition
                    complete-stx
                    scaffold-page-stx
                    register-stx
                    page->body)
  (with-syntax ([scaffold-page   scaffold-page-stx]
                [controller-set! register-stx])
    (syntax-parse complete-stx
                  
                  ; (_ (id arg ...)
                  ;   #:entity entity
                  ;   [#:page page]
                  ;   [#:other-kw other-val] ...
                  ;   [expr ...])
                  [(_ (~describe "function name and arguments" (id:id arg ...))
                      (~or (~once (~seq #:entity entity) #:name "#:entity argument" #:too-few "missing #:entity argument" #:too-many "multiple #:entity arguments")
                           (~optional (~seq #:page page) #:name "#:page argument" #:too-many "multiple #:page arguments")
                           (~seq kw:keyword val:expr)) ... body-expr:expr ...)
                   
                   (with-syntax* ([page-id                     (make-id #f #'id '-page)]
                                  [page                        (or (attribute page) #'(scaffold-page entity))]
                                  [(kw+val ...)                (apply append (map list
                                                                                  (syntax->list #'(kw ...))
                                                                                  (syntax->list #'(val ...))))]
                                  [define-page                 (if (pair? (attribute body-expr))
                                                                   #'(begin)
                                                                   #'(define page-id page))]
                                  [body                        (if (pair? (attribute body-expr))
                                                                   #'(begin body-expr ...)
                                                                   (page->body #'entity #'page-id))])
                     
                     (syntax/loc complete-stx
                       (begin define-page
                              (define-controller (id arg ...) kw+val ... (let ([id body]) body))
                              (controller-set! entity id))))]
                  
                  ; (_ id entity
                  ;   [#:page page]
                  ;   [#:other-kw other-val] ...)
                  [(_ id:id 
                      (~and entity:id (~fail #:unless (entity-info-ref #'entity) "missing entity"))
                      (~or (~optional (~seq #:page page) #:name "#:page argument" #:too-many "multiple #:page arguments")
                           (~seq kw:keyword val:expr)) ...)
                   
                   (with-syntax* ([page-id                          (make-id #f #'id '-page)]
                                  [page                             (or (attribute page) #'(scaffold-page entity))]
                                  [(kw+val ...)                     (apply append (map list
                                                                                       (syntax->list #'(kw ...))
                                                                                       (syntax->list #'(val ...))))]
                                  [define-page                      #'(define page-id page)]
                                  [body                             (page->body #'entity #'page-id)])
                     
                     (syntax/loc complete-stx
                       (begin define-page
                              (define-controller id kw+val ... body)
                              (controller-set! entity id))))])))

; Syntax -----------------------------------------

(define-syntax (define-report-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-report-page
   #'report-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(scaffold-report-controller-body entity #:page page)))))

(define-syntax (define-create-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-create-page
   #'create-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(scaffold-create-controller-body entity #:page page)))))

(define-syntax (define-review-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-review-page
   #'review-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(scaffold-review-controller-body entity #:page page)))))

(define-syntax (define-update-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-update-page
   #'update-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(scaffold-update-controller-body entity #:page page)))))

(define-syntax (define-delete-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-delete-page
   #'delete-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(scaffold-delete-controller-body entity #:page page)))))

; Controller bodies ------------------------------

; entity [html-page%] -> (-> void)
(define (scaffold-report-controller-body
         entity
         #:page   [page (scaffold-report-page entity)])
  (lambda ()
    (send* page [respond])))

; entity [html-page%] [(snooze-struct -> any)] -> (-> void)
(define (scaffold-create-controller-body
         entity
         #:page   [page              (scaffold-create-page entity)]
         #:review [review-controller (review-controller-ref entity)])
  (lambda ()
    (review-controller
     (send* page
       [set-value! (make-snooze-struct/defaults entity)]
       [respond]))))

; entity [html-page%] [(-> any)] -> (snooze-struct -> void)
(define (scaffold-review-controller-body
         entity
         #:page   [page              (scaffold-review-page entity)]
         #:report [report-controller (report-controller-ref entity)])
  (lambda (struct)
    (if struct
        (begin (send* page
                 [set-value! struct] 
                 [respond]))
        (begin (notifications-add! (xml "The " ,(entity-pretty-name entity)
                                        " you requested could not be found."))
               (report-controller)))))

; entity [html-page%] [(scnooze-struct -> any)] [(-> any)] -> (snooze-struct -> void)
(define (scaffold-update-controller-body
         entity
         #:page   [page              (scaffold-update-page entity)]
         #:review [review-controller (review-controller-ref entity)]
         #:report [report-controller (report-controller-ref entity)])
  (lambda (struct)
    (if struct
        (begin (review-controller 
                (send* page 
                  [set-value! struct] 
                  [respond])))
        (begin (notifications-add! (xml "The " ,(entity-pretty-name entity)
                                        " you requested could not be found."))
               (report-controller)))))

; entity [html-page%] [(-> any)] -> (snooze-struct -> void)
(define (scaffold-delete-controller-body
         entity
         #:page   [page              (scaffold-delete-page entity)]
         #:report [report-controller (report-controller-ref entity)])
  (lambda (struct)
    (if struct
        (begin (send* page 
                 [set-value! struct] 
                 [respond])
               (report-controller))
        (begin (notifications-add! (xml "The " ,(entity-pretty-name entity)
                                        " you requested could not be found."))
               (report-controller)))))

; Provide statements -----------------------------

(provide (all-from-out "controller-internal.ss")
         define-report-controller
         define-create-controller
         define-review-controller
         define-update-controller
         define-delete-controller)

(provide/contract
 [scaffold-report-controller-body (->* (entity?)
                                       (#:page (is-a?/c html-page%))
                                       procedure?)]
 [scaffold-create-controller-body (->* (entity?)
                                       (#:page (is-a?/c html-page%)
                                               #:review procedure?)
                                       procedure?)]
 [scaffold-review-controller-body (->* (entity?)
                                       (#:page (is-a?/c html-page%)
                                               #:report procedure?)
                                       procedure?)]
 [scaffold-update-controller-body (->* (entity?)
                                       (#:page (is-a?/c html-page%)
                                               #:review procedure?
                                               #:report procedure?)
                                       procedure?)]
 [scaffold-delete-controller-body (->* (entity?)
                                       (#:page (is-a?/c html-page%)
                                               #:report procedure?)
                                       procedure?)])