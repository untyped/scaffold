#lang scheme/base

(require "base.ss")

(require "site.ss"
         "model/db.ss"
         "model/entities.ss")

; Configuration ----------------------------------

(default-controller-wrapper
  (lambda (controller . args)
    (init-smoke (cut apply plain-controller-wrapper controller args))))

; Provide statements -----------------------------

(provide (all-from-out "base.ss"
                       "site.ss"
                       "model/db.ss"
                       "model/entities.ss"))
