#lang scheme/base

(require "base.ss")

(require srfi/13
         (unlib-in date for)
         (only-in (unlib-in time) date-day-of-the-week))

; Search patterns --------------------------------

; string [boolean] -> string
(define (pattern->regexp pattern [anywhere? #f])
  (for/fold1 ([accum      (if anywhere? "" "^")]
              [in-escape? #f])
             ([chr        (in-list (string->list pattern))])
             (case chr
               [(#\\)  (if in-escape?
                           (values (string-append accum (regexp-quote "\\")) #f)
                           (values accum #t))]
               [(#\*) (if in-escape?
                          (values (string-append accum (regexp-quote "*")) #f)
                          (values (string-append accum ".*") #f))]
               [(#\?)  (if in-escape?
                           (values (string-append accum (regexp-quote "?")) #f)
                           (values (string-append accum ".") #f))]
               [else  (values (string-append accum (regexp-quote (string chr))) #f)])))

; string [#:tz string] [#:now time-utc] -> (U time-utc #f)
(define (pattern->time pattern #:tz [tz (current-tz)] #:now [now (current-time time-utc)])
  
  ; -> date
  (define (today)
    (let ([now (time-utc->date now #:tz tz)])
      (make-date 0 00 00 00 (date-day now) (date-month now) (date-year now) #:tz tz)))
  
  ; symbol -> date
  (define (last day-of-the-week)
    (let loop ([now (date+days (today) -1)])
      (if (equal? (date-day-of-the-week now) day-of-the-week)
          now
          (loop (date+days now -1)))))
  
  (let* ([pattern (string-downcase pattern)])
    (with-handlers ([exn? (lambda (exn) #f)])
      (cond [(equal? pattern "today")              (date->time-utc (today))]
            [(equal? pattern "yesterday")          (date->time-utc (date+days (today) -1))]
            [(member pattern '("mon" "monday"))    (date->time-utc (last 'mon))]
            [(member pattern '("tue" "tuesday"))   (date->time-utc (last 'tue))]
            [(member pattern '("wed" "wednesday")) (date->time-utc (last 'wed))]
            [(member pattern '("thu" "thursday"))  (date->time-utc (last 'thu))]
            [(member pattern '("fri" "friday"))    (date->time-utc (last 'fri))]
            [(member pattern '("sat" "saturday"))  (date->time-utc (last 'sat))]
            [(member pattern '("sun" "sunday"))    (date->time-utc (last 'sun))]
            [(regexp-match #rx"([0-9][0-9]?)/([0-9][0-9]?)" pattern)
             => (match-lambda 
                  [(list _ day month)
                   (let* ([now (today)]
                          [ans (make-date (date-nanosecond now)
                                          (date-second     now)
                                          (date-minute     now)
                                          (date-hour       now)
                                          (string->number  day)
                                          (string->number  month)
                                          (date-year       now)
                                          #:tz tz)])
                     (date->time-utc
                      (if (time>? (date->time-utc ans) (date->time-utc now))
                          (date+years ans -1)
                          ans)))])]
            [(regexp-match #rx"([0-9][0-9]?):([0-9][0-9]?)" pattern)
             => (match-lambda 
                  [(list _ hour min)
                   (let* ([now (today)]
                          [ans (make-date (date-nanosecond now)
                                          (date-second     now)
                                          (string->number  min)
                                          (string->number  hour)
                                          (date-day        now)
                                          (date-month      now)
                                          (date-year       now)
                                          #:tz tz)])
                     (date->time-utc
                      (if (time>? (date->time-utc ans) (date->time-utc now))
                          (date+days ans -1)
                          ans)))])]
            [else #f]))))

; Provides ---------------------------------------

(provide/contract
 [pattern->regexp (->* (string?) (boolean?) string?)]
 [pattern->time   (->* (string?) (#:tz zone-exists? #:now time-utc?) (or/c time-utc? #f))])