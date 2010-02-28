#lang scheme/base

(require (planet untyped/unlib:3/require))

(define-library-aliases cce-scheme (planet cce/scheme:6)            #:provide)
(define-library-aliases schemeunit (planet schematics/schemeunit:3) #:provide)
(define-library-aliases delirium   (planet untyped/delirium:3)      #:provide)
(define-library-aliases dispatch   (planet untyped/dispatch:3)      #:provide)
(define-library-aliases mirrors    (planet untyped/mirrors:2)       #:provide)
(define-library-aliases smoke      (planet untyped/smoke:1)         #:provide)
(define-library-aliases snooze     (planet untyped/snooze:3)        #:provide)
(define-library-aliases unlib      (planet untyped/unlib:3)         #:provide)

; Requires ---------------------------------------

(require scheme/contract
         scheme/list
         scheme/match
         scheme/pretty
         scheme/runtime-path
         srfi/26
         (dispatch-in)
         (mirrors-in)
         (smoke-in)
         (snooze-in)
         (unlib-in debug enumeration exn for log))

; Provides ---------------------------------------

(provide (dispatch-out)
         (mirrors-out)
         (smoke-out)
         (snooze-out)
         (unlib-out debug enumeration exn for log)
         (all-from-out scheme/contract
                       scheme/list
                       scheme/match
                       scheme/pretty
                       srfi/26))
