#lang scheme/base

(require "../content-base.ss")

; Controllers ------------------------------------

(define-report-controller comment-report comment)
(define-review-controller comment-review comment)
(define-create-controller comment-create comment)
(define-update-controller comment-update comment)
(define-delete-controller comment-delete comment)
