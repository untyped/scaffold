#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base
                     (unlib-in syntax)))

; Caches -----------------------------------------

; (hashof entity controller)
(define report-controllers (make-hasheq))
(define create-controllers (make-hasheq))
(define review-controllers (make-hasheq))
(define update-controllers (make-hasheq))
(define delete-controllers (make-hasheq))

; controller-ref ---------------------------------

(define-syntax-rule (define/provide-controller-ref id type cache)
  (begin (define (id struct+entity [default (cut error (format "~a controller not found" type) struct+entity)])
           (hash-ref cache (struct+entity->entity struct+entity) default))
         (provide/contract [id (->* ((or/c snooze-struct? entity?)) (any/c) any)])))

; (U snooze-struct entity) -> controller
(define/provide-controller-ref report-controller-ref "report" report-controllers)
(define/provide-controller-ref create-controller-ref "create" create-controllers)
(define/provide-controller-ref review-controller-ref "review" review-controllers)
(define/provide-controller-ref update-controller-ref "update" update-controllers)
(define/provide-controller-ref delete-controller-ref "delete" delete-controllers)

; controller-set? --------------------------------

(define-syntax-rule (define/provide-controller-set? id cache)
  (begin (define (id struct+entity)
           (and (hash-ref cache (struct+entity->entity struct+entity) #f) #t))
         (provide/contract [id (-> (or/c snooze-struct? entity?) boolean?)])))

; (U snooze-struct entity) -> controller
(define/provide-controller-set? report-controller-set? report-controllers)
(define/provide-controller-set? create-controller-set? create-controllers)
(define/provide-controller-set? review-controller-set? review-controllers)
(define/provide-controller-set? update-controller-set? update-controllers)
(define/provide-controller-set? delete-controller-set? delete-controllers)

; controller-set! --------------------------------

(define-syntax-rule (define/provide-controller-set! id cache)
  (begin (define (id entity controller)
           (hash-set! cache entity controller))
         (provide/contract [id (-> entity? procedure? void?)])))

; entity controller -> void
(define/provide-controller-set! report-controller-set! report-controllers)
(define/provide-controller-set! create-controller-set! create-controllers)
(define/provide-controller-set! review-controller-set! review-controllers)
(define/provide-controller-set! update-controller-set! update-controllers)
(define/provide-controller-set! delete-controller-set! delete-controllers)

; controller-url ---------------------------------

(define-syntax-rule (define/provide-controller-url/entity id controller-ref)
  (begin (define (id entity)
           (controller-url (controller-ref entity)))
         (provide/contract [id (-> entity? string?)])))

(define-syntax-rule (define/provide-controller-url/struct id controller-ref)
  (begin (define (id struct)
           (controller-url (controller-ref (snooze-struct-entity struct)) struct))
         (provide/contract [id (-> snooze-struct? string?)])))

; entity -> string
(define/provide-controller-url/entity report-controller-url report-controller-ref)
(define/provide-controller-url/entity create-controller-url create-controller-ref)

; snooze-struct -> string
(define/provide-controller-url/struct review-controller-url review-controller-ref)
(define/provide-controller-url/struct update-controller-url update-controller-ref)
(define/provide-controller-url/struct delete-controller-url delete-controller-ref)

; call-controller --------------------------------

(define-syntax-rule (define/provide-call-controller/entity id controller-ref)
  (begin (define (id entity)
           ((controller-ref entity)))
         (provide/contract [id (-> entity? any)])))

(define-syntax-rule (define/provide-call-controller/struct id controller-ref)
  (begin (define (id struct)
           ((controller-ref (snooze-struct-entity struct)) struct))
         (provide/contract [id (-> snooze-struct? any)])))

; entity -> any
(define/provide-call-controller/entity call-report-controller report-controller-ref)
(define/provide-call-controller/entity call-create-controller create-controller-ref)

; snooze-struct -> any
(define/provide-call-controller/struct call-review-controller review-controller-ref)
(define/provide-call-controller/struct call-update-controller update-controller-ref)
(define/provide-call-controller/struct call-delete-controller delete-controller-ref)

; controller-link --------------------------------

(define-syntax-rule (define/provide-controller-link/entity id type pretty-name-ref controller-ref)
  (begin (define (id entity
                     #:body    [body        (format "~a ~a" (string-titlecase type) (pretty-name-ref entity))]
                     #:id      [id          #f]
                     #:class   [class       #f]
                     #:classes [classes     (if class (list class) null)]
                     #:title   [title       #f]
                     #:format  [link-format (default-link-format)]
                     #:else    [substitute  (default-link-substitute)])
           (controller-link (controller-ref entity)
                            #:body    body
                            #:id      id
                            #:class   class
                            #:classes classes
                            #:title   title
                            #:format  link-format
                            #:else    substitute))
         (provide/contract [id (->* (entity?)
                                    (#:body (or/c xml+quotable? pair? null? #f)
                                            #:id      (or/c symbol? string? #f)
                                            #:class   (or/c symbol? string? #f)
                                            #:classes (listof (or/c symbol? string?))
                                            #:title   (or/c string? #f)
                                            #:format  (enum-value/c link-formats)
                                            #:else    any/c)
                                    any)])))

(define-syntax-rule (define/provide-controller-link/struct id type controller-ref)
  (begin (define (id struct
                     #:body    [body        (format "~a ~a" (string-titlecase type) (format-snooze-struct struct))]
                     #:id      [id          #f]
                     #:class   [class       #f]
                     #:classes [classes     (if class (list class) null)]
                     #:title   [title       #f]
                     #:format  [link-format (default-link-format)]
                     #:else    [substitute  (default-link-substitute)])
           (controller-link (controller-ref struct) struct
                            #:body    body
                            #:id      id
                            #:class   class
                            #:classes classes
                            #:title   title
                            #:format  link-format
                            #:else    substitute))
         (provide/contract [id (->* (snooze-struct?)
                                    (#:body (or/c xml+quotable? pair? null? #f)
                                            #:id      (or/c symbol? string? #f)
                                            #:class   (or/c symbol? string? #f)
                                            #:classes (listof (or/c symbol? string?))
                                            #:title   (or/c string? #f)
                                            #:format  (enum-value/c link-formats)
                                            #:else    any/c)
                                    any)])))

; entity -> any
(define/provide-controller-link/entity report-controller-link "list"   entity-pretty-name-plural report-controller-ref)
(define/provide-controller-link/entity create-controller-link "new"    entity-pretty-name        create-controller-ref)

; snooze-struct -> any
(define/provide-controller-link/struct review-controller-link "view"   review-controller-ref)
(define/provide-controller-link/struct update-controller-link "edit"   update-controller-ref)
(define/provide-controller-link/struct delete-controller-link "delete" delete-controller-ref)

; Helpers ----------------------------------------

; (U snooze-struct entity) -> entity
(define (struct+entity->entity struct+entity)
  (if (entity? struct+entity)
      struct+entity
      (snooze-struct-entity struct+entity)))
