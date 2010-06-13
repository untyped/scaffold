#lang scheme/base

(require "base.ss")

(require (unlib-in symbol)
         "attribute-editor-internal.ss"
         "check-label.ss"
         "editor-internal.ss"
         "foreign-key-editor.ss"
         "relationship-editor.ss")

; Helper mixins ----------------------------------

(define text-field-editor-mixin
  (mixin/cells (text-input<%> editor<%>) ()
    (init [attributes null]
          [max-length (or (and (pair? attributes)
                               (character-type? (attribute-type (car attributes)))
                               (character-type-max-length (attribute-type (car attributes))))
                          25)]
          [size       (default-text-field-size max-length)])
    (super-new [attributes attributes]
               [max-length max-length]
               [size       size])))

(define time-utc-editor-mixin
  (mixin/cells (date-field<%> editor<%>) ()
    (inherit get-time-utc)
    ; -> (U time-utc #f)
    (define/override (get-value)
      (get-time-utc))))

(define time-tai-editor-mixin
  (mixin/cells (date-field<%> editor<%>) ()
    (inherit get-time-tai)
    ; -> (U time-tai #f)
    (define/override (get-value)
      (get-time-tai))))

(define symbol-editor-mixin
  (mixin/cells (form-element<%> editor<%>) ()
    ; -> (U symbol #f)
    (define/override (get-value)
      (let ([str (super get-value)])
        (and str (string->symbol str))))
    ; (U symbol #f) -> void
    (define/override (set-value! sym)
      (super set-value! (and sym (symbol->string sym))))))

(define enum-editor-mixin
  (mixin/cells (attribute-editor<%>) ()
    
    (init attributes
          [null-label (if (is-a? this radio-combo%)
                          "None"
                          "-- No selection --")]
          [options    (let* ([type (and (pair? attributes) (attribute-type (car attributes)))])
                        (if (enum-type? type)
                            (enum-type-options type null-label)
                            (raise-exn exn:fail:contract
                              (format "enum-combo-box-editor% constructor: ~a: ~s"
                                      "options must be specified for non-enum attributes"
                                      attributes))))])
    
    (super-new [attributes attributes] [options options])))

; Classes ----------------------------------------

(define autocomplete-editor%              (complete-attribute-editor-mixin autocomplete-field%))
(define check-box-editor%                 (attribute-editor-mixin (check-label-mixin (simple-editor-mixin check-box%))))
(define combo-box-editor%                 (enum-editor-mixin (complete-attribute-editor-mixin combo-box%)))
(define vanilla-combo-box-editor%         (complete-attribute-editor-mixin vanilla-combo-box%))
(define date-editor%                      (complete-attribute-editor-mixin date-field%))
(define file-editor%                      (complete-attribute-editor-mixin file-field%))
(define integer-editor%                   (complete-attribute-editor-mixin integer-field%))
(define number-editor%                    (complete-attribute-editor-mixin number-field%))
(define password-editor%                  (complete-attribute-editor-mixin password-field%))
(define radio-combo-editor%               (enum-editor-mixin (complete-attribute-editor-mixin radio-combo%)))
(define regexp-editor%                    (complete-attribute-editor-mixin regexp-field%))
(define set-selector-combo-box-editor%    (complete-attribute-editor-mixin set-selector-combo-box%))
(define set-selector-autocomplete-editor% (complete-attribute-editor-mixin set-selector-autocomplete%))
(define text-field-editor%                (text-field-editor-mixin (complete-attribute-editor-mixin text-field%)))
(define text-area-editor%                 (complete-attribute-editor-mixin text-area%))
(define tiny-mce-editor%                  (complete-attribute-editor-mixin tiny-mce%))

; Procedures -------------------------------------

; (parameter (attribute -> attribute-editor<%>))
(define attribute-editor-defaults
  (make-parameter
   (lambda (attr)
     (let* ([entity (attribute-entity attr)]
            [type   (attribute-type   attr)])
       (match type
         [(? guid-type?)     (new foreign-key-editor% [attributes (list attr)] [entity (guid-type-entity type)])]
         [(? boolean-type?)  (new check-box-editor%   [attributes (list attr)] [show-label? #f])]
         [(? integer-type?)  (new integer-editor%     [attributes (list attr)])]
         [(? enum-type?)     (if (< (length (enum-type-values type)) 5)
                                            (new radio-combo-editor% [attributes (list attr)] [vertical? #f])
                                            (new combo-box-editor%   [attributes (list attr)]))]
         [(? real-type?)     (new number-editor%      [attributes (list attr)])]
         [(? time-utc-type?) (new (time-utc-editor-mixin date-editor%) [attributes (list attr)])]
         [(? time-tai-type?) (new (time-tai-editor-mixin date-editor%) [attributes (list attr)])]
         [(struct string-type (_ max-length))
          (if max-length
              (new text-field-editor%
                   [attributes (list attr)]
                   [size (default-text-field-size max-length)]
                   [max-length max-length])
              (new text-area-editor%
                   [attributes (list attr)]
                   [cols 50]
                   [rows 5]))]
         [(struct symbol-type (_ max-length))
          (if max-length
              (new (symbol-editor-mixin text-field-editor%)
                   [attributes (list attr)]
                   [size (default-text-field-size max-length)]
                   [max-length max-length])
              (new (symbol-editor-mixin text-area-editor%)
                   [attributes (list attr)]
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

(provide (all-from-out "attribute-editor-internal.ss"
                       "foreign-key-editor.ss"
                       "relationship-editor.ss")
         time-utc-editor-mixin
         time-tai-editor-mixin
         symbol-editor-mixin
         enum-editor-mixin
         autocomplete-editor%
         check-box-editor%
         combo-box-editor%
         vanilla-combo-box-editor%
         date-editor%
         file-editor%
         integer-editor%
         number-editor%
         password-editor%
         radio-combo-editor%
         regexp-editor%
         set-selector-combo-box-editor%
         set-selector-autocomplete-editor%
         text-field-editor%
         text-area-editor%
         tiny-mce-editor%)

(provide/contract
 [attribute-editor-defaults (parameter/c procedure?)]
 [default-attribute-editor  (-> attribute? (is-a?/c attribute-editor<%>))])
