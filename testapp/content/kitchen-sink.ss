#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3)
         "kitchen-sink-view-variants.ss")

; Controllers ------------------------------------

(define-report-controller sink-report kitchen-sink)
(define-review-controller sink-review kitchen-sink)
(define-create-controller sink-create kitchen-sink)
(define-update-controller sink-update kitchen-sink)
(define-delete-controller sink-delete kitchen-sink)
