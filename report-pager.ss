#lang scheme/base

(require "base.ss")

(require (only-in srfi/1 filter)
         srfi/13
         srfi/19
         (unlib-in list symbol)
         "report-column.ss"
         "report-view.ss"
         "util.ss")

; Components -------------------------------------

(define self-hiding-combo-box%
  (class/cells combo-box% ()
    (inherit get-options)
    (define/override (get-enabled?)
      (and (list-ref? (get-options) 1)
           (super get-enabled?)))))

; Interfaces -------------------------------------

(define report-pager<%>
  (interface ()
    get-start
    set-start!
    get-count
    set-count!
    get-total
    set-total!
    init!
    render-pager))

; Mixins -----------------------------------------

(define report-pager-mixin
  (mixin/cells (html-element<%>) (report-pager<%>)
    (inherit core-html-attributes)
    
    ; (cell integer)
    (init-cell start 0 #:accessor #:mutator)
    
    ; (cell integer)
    (init-cell count 1000000 #:accessor #:mutator)
    
    ; (cell integer)
    (init-cell total 1000000 #:accessor #:mutator)
    
    ; integer integer integer -> void
    (define/public (init! start count total)
      (error "init! must be overridden"))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (div (@ ,@(core-html-attributes seed))
                ,(render-pager seed (get-start) (get-count) (get-total)))))
    
    ; seed integer integer integer -> xml
    (define/public (render-pager seed start count total)
      (error "render-pager must be overridden"))
    
    ;  seed
    ;  integer
    ;  (U integer string)
    ;  [#:title (U string #f)]
    ;  [#:active?   boolean]
    ;  [#:disabled? boolean]
    ; ->
    ;  xml
    (define/public (link seed to text #:title [title #f] #:active? [active? #f] #:disabled? [disabled? #f])
      (xml (td (@ [class ,(cond [active?   'ui-state-active]
                                [disabled? "ui-state-default ui-state-disabled"]
                                [else      'ui-state-default])]
                  ,(opt-xml-attr title))
               (a (@ [onclick ,(embed/ajax seed (callback on-page to))]) ,text))))))

; Elements ---------------------------------------

(define quick-report-pager%
  (class/cells (report-pager-mixin html-element%) ()
    
    (inherit link get-id get-start set-start! get-count set-count! get-total core-html-attributes) 
    
    ; (listof integer)
    (init-field viewport-sizes (list 10 50 100) #:accessor)
    
    ; boolean
    (init-field allow-show-all? #t #:accessor)
    
    (field pager-start-field 
      (new integer-field% 
           [id        'pager-start]
           [min-value 1]
           [on-change (callback on-start-count-update)])
      #:child #:accessor)
    
    (field pager-count-field 
      (new self-hiding-combo-box% 
           [id        'pager-count]
           [on-change (callback on-start-count-update)])
      #:child #:accessor)
    
    (super-new)
    
    ; Paging -----------------------------------
    
    ; integer boolean -> void
    (define/public (set-start-value! start update-field?)
      (set-start! start)
      (when update-field? (send pager-start-field set-value! (add1 start))))
    
    ; integer boolean -> void
    (define/public (set-count-value! count update-field?)
      (set-count! count)
      (when update-field? (send pager-count-field set-value! count)))
    
    ; integer integer -> void
    (define/public #:callback (on-start-count-update)
      ; reset the value if it has changed to a bad value
      (unless (send pager-start-field value-valid?)
        (set-start-value! (get-start) #t))
      (page-to (sub1 (send pager-start-field get-value))
               (send pager-count-field get-value)))
    
    ; integer integer -> void
    (define/public #:callback (on-page start [count (get-count)])
      (page-to start count #t))
    
    ; integer integer [boolean] -> void
    (define/private (page-to start count [update-fields? #f])
      (set-start-value! start update-fields?)
      (set-count-value! count update-fields?)
      ; redo the options
      (let* ([total             (get-total)]
             [available-options (for/list ([increment (in-list (get-viewport-sizes))])
                                  (cons increment (+ start increment)))]
             [options           (let-values ([(options seen?)
                                              (for/fold ([options     null]
                                                         [total-seen? #f])
                                                        ([opt+inc (in-list available-options)])
                                                        (match-let ([(cons opt inc) opt+inc])
                                                          (cond [(and total-seen? (>= inc total))
                                                                 (values options total-seen?)]
                                                                [(>= inc total)
                                                                 (values (cons (cons opt total) options) #t)]
                                                                [else
                                                                 (values (cons opt+inc options) total-seen?)])))])
                                  (reverse (if (and (not seen?) allow-show-all?) (cons (cons #f total) options) options)))])
        (send pager-count-field set-options! options)))
    
    ; Rendering ----------------------------------
    
    ; integer integer integer -> void
    (define/override (init! start count total)
      (set-total! total)
      (page-to start count #t))
    
    (define/override (set-total! total)
      (super set-total! total)
      (send* pager-start-field
        [set-max-value! total]
        [set-size!      (string-length (number->string total))]))
    
    ; seed integer integer integer -> xml
    (define/override (render-pager seed start count total)
      (let* ([last-item    ; integer: index (within report) of first on final page
              (max 0 (- (sub1 total) (if count (remainder (sub1 total) count) (sub1 total))))] 
             [last-page    ; integer: index (within pager) of last page
              (add1 (ceiling (if count (/ last-item count) 0)))]
             [current-page ; integer: index (within pager) of current page
              (floor (if count (/ start count) 0))]
             [single-page? (and (zero? current-page) (not (< current-page (sub1 last-page))))])
        (send pager-start-field set-visible?! (not single-page?))
        (send pager-count-field set-visible?! (not single-page?))
        ; xml 
        ; If we are displaying all items in the list, there is no need for a pager:
        ; Otherwise, output a list of links like this:
        ;   |< < [1] to [10 v] of 100 > >|
        ; for each page that can be viewed.
        (xml (table (@ [class "pager"]) 
                    (tbody (tr ,(link seed 
                                      0 
                                      (xml (& laquo))
                                      #:title     "Jump to page 1"
                                      #:disabled? (zero? current-page))
                               ,(link seed 
                                      (if count (max 0 (- start count)) 0)
                                      (xml (& lsaquo)) 
                                      #:title     "Previous page"
                                      #:disabled? (zero? current-page))
                               ,(if single-page?
                                    (xml (td (@ [class "pager-pages"])
                                             ,(if (= total 1) "1 item" (format "~a items" total))))
                                    (xml (td (@ [class "pager-pages"])
                                             ,(send pager-start-field render seed)
                                             " to "
                                             ,(send pager-count-field render seed)
                                             " of " ,total)))
                               ,(link seed 
                                      (if count (min last-item (+ start count)) 0)
                                      (xml (& rsaquo))
                                      #:title     "Next page"
                                      #:disabled? (not (< current-page (sub1 last-page))))
                               ,(link seed 
                                      last-item 
                                      (xml (& raquo))
                                      #:title     (string-append "Jump to page " (number->string last-page))
                                      #:disabled? (not (< current-page (sub1 last-page))))))))))))

(define linked-report-pager%
  (class/cells (report-pager-mixin html-element%) ()
    
    (inherit link set-count! set-start! set-total! get-id core-html-attributes)
    
    ; (cell integer): number of pages added to left and right in pager
    (init-cell pager-cell-count 4 #:accessor #:mutator)
    
    ; Paging -----------------------------------
    
    ; integer -> void
    (define/public #:callback (on-page start)
      (set-start! start))
    
    
    ; integer integer integer -> void
    (define/override (init! start count total)
      (set-start! start)
      (set-total! total)
      (set-count! count))
    
    ; Rendering ----------------------------------
    
    ; seed integer integer integer -> xml
    (define/override (render-pager seed start count total)
      (let*-values 
          ([(last-item)                        ; integer: index (within report) of first on final page
            (max 0 (- (sub1 total) (if count (remainder (sub1 total) count) (sub1 total))))] 
           [(last-page)                        ; integer: index (within pager) of last page
            (add1 (ceiling (if count (/ last-item count) 0)))]
           [(current-page)                     ; integer: index (within pager) of current page
            (floor (if count (/ start count) 0))]
           [(pager-first-page pager-last-page) ; integers: page-index limits of pager-window
            (calculate-pager-limits current-page last-page (get-pager-cell-count))]
           [(lowest-item)                      ; integer: index (within report) of first-page first-item
            (max (if count (* pager-first-page count) 0) 0)]
           [(highest-item)                     ; integer: index (within report) of last-page first-item
            (min (if count (* pager-last-page count) 0) total)]) 
        
        ; xml      
        ; If we are displaying all items in the list, there is no need for a pager:
        (opt-xml (not (or (zero? total) (not count) (>= count total)))
          ; Otherwise, output a list of links like this:
          ;   < Prev 1 2 Next >
          ; for each page that can be viewed.
          (div (@ [class "pager-and-position"])
               (div (@ [class "position"])
                    (div (@ [class "item-count"])
                         "Displaying items " ,(max 1 (min total (if start (add1 start) 1)))
                         " to " ,(max 1 (min total (cond [(and start count) (+ start count)]
                                                         [count count]
                                                         [else total])))
                         " of " ,total "."))
               (table (@ [class "pager"]) 
                      (tbody (tr ,(link seed 0 (xml (& laquo))
                                        #:title "Jump to page 1"
                                        #:disabled? (zero? current-page))
                                 ,(link seed (max 0 (- start count))
                                        (xml (& lsaquo)) 
                                        #:title "Previous page"
                                        #:disabled? (zero? current-page))
                                 ; ellipsis
                                 ,(if (not (zero? pager-first-page)) 
                                      (xml (td (@ [class 'spacer]) (span "...")))
                                      (xml (td (@ [class 'spacer]) (span (& nbsp)))))
                                 ,@(for/list ([i (in-range lowest-item highest-item count)])
                                     (link seed i (add1 (/ i count)) #:active? (and (>= i start) (< i (+ start count)))))
                                 ; ellipsis
                                 ,(if (not (= last-page pager-last-page)) 
                                      (xml (td (@ [class 'last]) (span "...")))
                                      (xml (td (@ [class 'last]) (span (& nbsp)))))
                                 ,(link seed (min last-item (+ start count))
                                        (xml (& rsaquo))
                                        #:title "Next page"
                                        #:disabled? (not (< current-page (sub1 last-page))))
                                 ,(link seed last-item (xml (& raquo))
                                        #:title (string-append "Jump to page " (number->string last-page))
                                        #:disabled? (not (< current-page (sub1 last-page)))))))))))))

; Helpers --------------------------------------

; integer integer integer -> (values integer integer)
(define (calculate-pager-limits current-page last-page window-offset)
  (let* ([unbounded-low-page  (- current-page window-offset)]
         [bounded-low-page    (max unbounded-low-page 0)]
         [unbounded-high-page (add1 (+ current-page window-offset))]
         [bounded-high-page   (min unbounded-high-page last-page)])
    
    ; integer: pager-window index of first page to display
    ; if there are unused window cells on high end, add to low
    (define window-first-page
      (let ([take-from-low (- unbounded-high-page bounded-high-page)] )
        (max (- bounded-low-page take-from-low) 0)))
    
    ; integer: pager-window index of last page to display
    ; if there are unused window cells on low end, add to high
    (define window-last-page
      (let ([add-to-high   (- bounded-low-page unbounded-low-page)])
        (min (+ bounded-high-page add-to-high) last-page)))
    
    (values window-first-page window-last-page)))

; Provide statements --------------------------- 

(provide report-pager<%>
         linked-report-pager%
         quick-report-pager%)