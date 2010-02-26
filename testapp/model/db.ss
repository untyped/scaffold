#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/snooze:3/postgresql8/postgresql8))

; Configuration ----------------------------------

(current-snooze (make-snooze (make-postgresql8-database #:database "scaffoldtest" #:username "scaffoldtest")))

; Provides ---------------------------------------

(provide (all-from-out (planet untyped/snooze:3)))
