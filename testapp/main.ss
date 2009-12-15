#lang scheme/base

(require (planet untyped/unlib:3/require))

(require scheme/cmdline
         "content-base.ss"
         "content/content.ss")

; -> void
(define (run-application)
  (serve/smoke (lambda ()
                 (site-dispatch test-site (current-request)))
               #:htdocs-paths (list testapp-htdocs-path)))

; -> void
(define (recreate-database)
  (for-each drop-table   (list post kitchen-sink))
  (for-each create-table (list post kitchen-sink)))

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
  [(initdb) (with-connection (recreate-database))])
