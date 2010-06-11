#lang scheme/base

(require "content-base.ss")

(require scheme/cmdline
         "content/content.ss")

; -> void
(define (run-application)
  (serve/smoke (lambda ()
                 (site-dispatch test-site (current-request)))
               #:htdocs-paths (list testapp-htdocs-path)))

; Main program body ------------------------------

; symbol
(define mode 'runapp)

; void
(command-line #:once-any 
              [("--test")   "Run in test mode."        (set! mode 'test)]
              [("--initdb") "Initialise the database." (set! mode 'initdb)])

; void
(case mode
  [(runapp) (run-application)])
