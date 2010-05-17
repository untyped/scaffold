#lang scheme/base

(require "../content-base.ss")

(define-class integer+string-editor% compound-editor% ()
  
  ; Fields ---------------------------------------
  ; editor% ...
  (field integer-editor (default-attribute-editor (attr kitchen-sink a-integer)) #:child)
  (field string-editor  (default-attribute-editor (attr kitchen-sink a-string))  #:child)
  
  ; Constructor ----------------------------------
  
  (super-new [sub-editors (list integer-editor string-editor)])
  
  ; Methods --------------------------------------
  
  ; seed -> xml
  (define/override (render seed)
    (xml "Str " ,(send string-editor render seed) " >> Integer " ,(send integer-editor render seed)))
  
  ; (list (U integer #f) (U string #f)) -> void
  (define/override (set-value! int+str)
    (match-let ([(list int str) int+str])
      (send integer-editor set-value! int)
      (send string-editor  set-value! str)))
  
  ; -> (list (U integer #f) (U string #f))
  (define/override (get-value)
    (list (send integer-editor get-value)
          (send string-editor  get-value)))
  
  ; set-raw! and get-raw are defined purely for unit tests:
  
  ; (list (U string #f) (U string #f)) -> void
  (define/public (set-raw! str+str)
    (match-let ([(list str1 str2) str+str])
      (send integer-editor set-raw! str1)
      (send string-editor set-raw! str2)))
  
  ; -> (list (U string #f) (U string #f))
  (define/public (get-raw)
    (list (send integer-editor get-raw)
          (send string-editor get-raw))))

; Provides ---------------------------------------

(provide integer+string-editor%)
