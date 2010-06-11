#lang scheme/base

(require "../base.ss")

(require (snooze-in main postgresql8/postgresql8))

; Configuration ----------------------------------

(current-snooze (make-snooze (make-postgresql8-database #:database "smoketest" #:username "smoketest")))

; Provides ---------------------------------------

(provide (snooze-out main postgresql8/postgresql8))
