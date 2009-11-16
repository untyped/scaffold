#lang scheme

(require "base.ss")

(require "report.ss"
         "scaffold/interfaces.ss"
         "scaffold/default-mixins.ss")

; Procedures -------------------------------------

(define (scaffold-report-element entity
                                 [report-class% (default-snooze-report-crudl-mixin snooze-report%)]
                                 #:crudl-mixin        [crudl-mixin        (default-crudl-mixin entity)]
                                 #:rdl-mixin          [rdl-mixin          (default-review+delete+list-mixin)]
                                 #:list-mixin         [list-mixin         (default-list-mixin)]
                                 #:crudl-report-mixin [crudl-report-mixin (default-crudl-report-mixin)]
                                 #:report-mixin       [report-mixin       (default-report-mixin report-class%)])
  (lambda (element)
    (report-mixin (crudl-report-mixin (list-mixin (rdl-mixin (crudl-mixin element)))))))


; Provides ---------------------------------------

(provide (all-defined-out)
         (all-from-out "scaffold/default-mixins.ss")
         (all-from-out "scaffold/interfaces.ss"))
