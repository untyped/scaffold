#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "editor-internal.ss"
         "foreign-key-editor.ss"
         "relationship-editor.ss")

; Helper mixins ----------------------------------

(define-mixin time-utc-field-mixin (date-field<%>) ()
  (inherit get-time-utc)
  ; -> (U time-utc #f)
  (define/override (get-value)
    (get-time-utc)))

(define-mixin time-tai-field-mixin (date-field<%>) ()
  (inherit get-time-tai)
  ; -> (U time-tai #f)
  (define/override (get-value)
    (get-time-tai)))

(define-mixin symbol-editor-mixin (form-element<%>) ()
  ; -> (U symbol #f)
  (define/override (get-value)
    (let ([str (super get-value)])
      (and str (string->symbol str))))
  ; (U symbol #f) -> void
  (define/override (set-value! sym)
    (super set-value! (and sym (symbol->string sym)))))


(define-mixin enum-field-mixin (form-element<%>) ()
  
  (init attribute
        [null-label (if (is-a? this radio-combo%)
                        "None"
                        "-- No selection --")]
        [options    (let* ([type (attribute-type attribute)])
                      (if (enum-type? type)
                          (enum-type-options type null-label)
                          (raise-exn exn:fail:contract
                            (format "enum-combo-box-editor% constructor: ~a: ~s"
                                    "options must be specified for non-enum attribute"
                                    attribute))))])
  
  (super-new [options options]))

; Procedures -------------------------------------

; (parameter (attribute -> form-element<%>))
(define attribute-editor-defaults
  (make-parameter
   (lambda (attr)
     (let* ([entity (attribute-entity attr)]
            [type   (attribute-type   attr)])
       (match type
         [(? guid-type?)     (new foreign-key-editor% [attribute attr] [entity (guid-type-entity type)])]
         [(? boolean-type?)  (new check-box% [show-label? #f])]
         [(? integer-type?)  (new integer-field%)]
         [(? enum-type?)     (if (< (length (enum-type-values type)) 5)
                                 (new (enum-field-mixin radio-combo%) [attribute attr] [vertical? #f])
                                 (new (enum-field-mixin combo-box%)   [attribute attr]))]
         [(? real-type?)     (new number-field%)]
         [(? time-utc-type?) (new (time-utc-field-mixin date-field%))]
         [(? time-tai-type?) (new (time-tai-field-mixin date-field%))]
         [(struct string-type (_ max-length))
          (if max-length
              (new text-field%
                   [size       (default-text-field-size max-length)]
                   [max-length max-length])
              (new text-area%
                   [cols 50]
                   [rows 5]))]
         [(struct symbol-type (_ max-length))
          (if max-length
              (new (symbol-editor-mixin text-field%)
                   [size       (default-text-field-size max-length)]
                   [max-length max-length])
              (new (symbol-editor-mixin text-area%)
                   [cols 50]
                   [rows 5]))]
         [(? binary-type?)
          (error "cannot scaffold attribute-editor for binary attribute" attr)]
         [_ (error "unrecognized attribute-type for default-attribute-editor" attr type)])))))

; attribute -> attribute-editor<%>
(define (default-attribute-editor attr)
  ((attribute-editor-defaults) attr))

; Helper procedures ------------------------------

; natural -> natural
(define (default-text-field-size max-length)
  ; Rounds up to the nearest 10 characters, within the bounds [0,50]:
  (max 10 (min 50 (* (add1 (floor (/ max-length 10))) 10))))

; enum-type [string] -> (alistof (U symbol #f) string)
(define (enum-type-options type [null-label "-- No selection --"])
  (let* ([enum          (enum-type-enum type)]
         [values        (enum-type-values type)]
         [value->string (if enum
                            (cut enum-prettify enum <>)
                            symbol->string)])
    `(,@(if (type-allows-null? type) `((#f . ,null-label)) null)
      ,@(for/list ([val (in-list values)])
          (cons val (value->string val))))))

; Provide statements -----------------------------

(provide (all-from-out "foreign-key-editor.ss"
                       "relationship-editor.ss"))

(provide/contract
 [attribute-editor-defaults (parameter/c procedure?)]
 [default-attribute-editor  (-> attribute? (is-a?/c form-element<%>))])
