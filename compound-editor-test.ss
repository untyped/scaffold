#lang scheme

(require "test-base.ss")

(require srfi/26
         "testapp/content/integer-string-editor.ss")

(define/provide-test-suite compound-editor-tests
  
  (test-suite "value-valid?, get-value-error"
    
    (test-case "blank"
      (let ([ed (new integer+string-editor%)])
        (check-equal? (send ed get-raw) '("" ""))
        (check-equal? (send ed get-value) '(#f #f))
        (check-equal? (send ed get-value-error) #f)
        (check-equal? (send ed value-valid?) #t)))
    
    (test-case "valid values"
      (let ([ed (new integer+string-editor%)])
        (send ed set-raw! '("1" "a"))
        (check-equal? (send ed get-raw) '("1" "a"))
        (check-equal? (send ed get-value) '(1 "a"))
        (check-equal? (send ed get-value-error) #f)
        (check-equal? (send ed value-valid?) #t)))
    
    (test-case "invalid values"
      (let ([ed (new integer+string-editor%)])
        (send ed set-raw! '("x" "a"))
        (check-not-exn
          (lambda ()
            (check-equal? (send ed get-raw) '("x" "a"))))
        (check-exn exn:smoke:form?
          (lambda ()
            (send ed get-value)))
        (check-not-exn
          (lambda ()
            (check-equal? (send ed get-value-error) "This value must be a whole number.")))
        (check-not-exn
          (lambda ()
            (check-equal? (send ed value-valid?) #f)))))))
