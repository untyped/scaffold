#lang scheme/base

(require (planet untyped/unlib:3/require))

(require scheme/cmdline
         "content-base.ss"
         "content/content.ss")

; -> void
(define (run-application)
  (serve/smoke (lambda ()
                 (with-connection
                   (site-dispatch test-site (current-request))))
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
  [(runapp) (run-application)]
  [(initdb) (with-connection (recreate-tables))])
