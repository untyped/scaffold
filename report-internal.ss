#lang scheme/base

(require "base.ss")

(require (only-in srfi/1 filter)
         srfi/13
         srfi/19
         (unlib-in list symbol)
         "report-column.ss"
         "report-pager.ss"
         "report-view.ss"
         "util.ss")

(define default-show-csv-link?
  (make-parameter #t))

(define-class snooze-report% html-element% ()
  
  (inherit get-id
           core-html-attributes)
  
  ; Fields -----------------------------------
  
  ; view-combo-box%
  (field view-field
    (new view-combo-box% [report this] [on-change (callback on-view-change)])
    #:child #:accessor)
  
  ; text-field%
  (field pattern-field
    (new text-field% [on-change (callback on-pattern-change)] [size 30] [placeholder "Type and press Enter to search"])
    #:child #:accessor)
  
  ; (cell (U snooze-report-column% #f))
  (init-cell sort-col #:accessor #:mutator)
  
  ; report-pager<%>
  (init-field pager-field 
    (new linked-report-pager%)
    #:child #:accessor)
  
  ; (cell (U 'asc 'desc))
  (init-cell sort-dir 'asc #:accessor #:mutator)
  
  ; boolean 
  (init-cell show-controls?        #t #:accessor #:mutator)
  (init-cell show-csv-link? 
             (default-show-csv-link?)
             #:accessor #:mutator)
  (init-cell show-pattern-field?   #t #:accessor #:mutator)
  (init-cell show-pager-top?       #t #:accessor #:mutator)
  (init-cell show-pager-bottom?    #t #:accessor #:mutator)
  
  ; Constructor ------------------------------
  
  ; (listof symbol)
  (init [classes null]
        [count   #f])
  
  (super-new [classes (list* 'smoke-snooze-report 'ui-widget classes)])
  
  ; (U view #f)
  (init [view (car (get-views))])
  (when view (send view-field set-value! view))
  
  (send pager-field set-count! count)
  
  ; Miscellaneous ------------------------------
  
  ; -> boolean
  (define/public (get-show-views?)
    (list-ref? (get-views) 1))
  
  (define/public (get-start)
    (send pager-field get-start))
  
  (define/public (set-start! start)
    (send pager-field set-start! start))
  
  (define/public (get-count)
    (send pager-field get-count))
  
  ; -> boolean 
  (define/override (dirty?) 
    (or (super dirty?) 
        (send view-field    dirty?)
        (send pattern-field dirty?)
        (send pager-field   dirty?))) 
  
  ; -> (listof (U xml (seed -> xml)))
  (define/augment (get-html-requirements)
    (list* snooze-styles
           (inner null get-html-requirements)))
  
  ; symbol -> void
  (define/override (set-id! id)
    (super set-id! id)
    (send view-field set-id! (symbol-append id '-view-field))
    (send pattern-field set-id! (symbol-append id '-pattern-field)))
  
  ; -> symbol
  (define/public (get-table-id)
    (symbol-append (get-id) '-table))
  
  ; Queries ----------------------------------
  
  ;  [#:start (U integer #f)]
  ;  [#:count (U integer #f)]
  ; ->
  ;  integer
  ;  integer
  ;  integer
  ;  (gen-> row-data)
  (define/public (do-queries #:start [start* (get-start)] #:count [count* (get-count)])
    ; Calculate the size of the viewport and number of items:
    (define pattern (send pattern-field get-value))
    (define total   (query-num-items pattern))
    (define start   (if start* (max 0 (min total start*)) #f))
    (define count   (if count* (max 0 count*) #f))
    ; Retrieve the items that are in the viewport:
    (define col     (get-sort-col))
    (define dir     (get-sort-dir))
    (define g:item  (query-items pattern col dir start count))
    ; Return the results:
    (values start count total g:item))
  
  ; what from where group -> integer
  (define/public (query-num-items)
    (error "query-num-items must be overridden."))
  
  ; string symbol (U 'asc 'desc) integer integer -> generator
  ;
  ; start and count arepassed in pre-adjusted for the size of the
  ; viewport and the number of available items.
  (define/public (query-items pattern col dir start count)
    (error "query-items must be overridden."))
  
  ; [(U snooze-report-column% #f)] [(U 'asc 'desc #f)] -> (listof order)
  ; 
  ; Generate the ORDER clause for the SQL query
  (define/public (get-sort-order [col (get-sort-col)] [dir (get-sort-dir)])
    (if col (send col get-order dir) null))
  
  ; Resorting --------------------------------
  
  ; string -> void
  (define/public #:callback (on-sort id-string)
    ; symbol
    (define id (string->symbol id-string))
    ; column
    (define col
      (ormap (lambda (col)
               (and (eq? id (send col get-id)) col))
             (get-all-columns)))
    (if (is-a? col snooze-report-column%)
        (resort! col)
        (error (format "No such column: ~s" id-string))))
  
  ; column -> void
  (define/public (resort! col)
    (define current-col (get-sort-col))
    (define current-dir (get-sort-dir))
    (if (eq? col current-col)
        (begin (set-sort-dir! (if (eq? current-dir 'asc)
                                  'desc
                                  'asc)))
        (begin (set-sort-col! col)
               (set-sort-dir! 'asc))))
  
  ; Columns and views --------------------------
  
  ; -> (listof view)
  (define/public (get-views)
    (error "get-views must be overridden."))
  
  ; -> view
  (define/public (get-current-view)
    (send view-field get-value))
  
  ; -> view
  (define/public (set-current-view-by-id! view-identifier)
    (send view-field set-value!
          (or (for/or ([view (in-list (get-views))])
                (and (eq? (view-id view) view-identifier) view))
              (send view-field get-value)))) 
  
  ; -> (listof column)
  (define/public (get-visible-columns)
    (filter (cut send <> get-display-in-html?)
            (cond [(send view-field get-value) => (cut view-columns <>)]
                  [(car (get-views))           => (cut view-columns <>)]
                  [else                           (get-all-columns)])))
  
  ; -> (listof column)
  (define/public (get-all-columns)
    (reverse 
     (for/fold ([accum null])
               ([view (get-views)])
               (for/fold ([accum accum])
                         ([col (view-columns view)])
                         (if (memq col accum)
                             accum
                             (cons col accum))))))
  
  ; -> void
  (define/public #:callback (on-view-change)
    ; Don't need to do anything here. 
    ; Everything is taken care of by the combo box changing state.
    (void))
  
  ; Filtering ----------------------------------
  
  ; -> void
  (define/public #:callback (on-pattern-change)
    (set-start! 0))
  
  ; Rendering ----------------------------------
  
  ; seed -> xml
  (define/override (render seed)
    (let-values ([(start count total g:item) (do-queries)]) ; integer integer integer (gen-> row-data)
      (xml (div (@ ,@(core-html-attributes seed))
                (div (@ [class "report-preamble ui-helper-clearfix"])  ,(render-preamble  seed start count total))
                ,(render-report seed start count total (get-visible-columns) g:item)
                (div (@ [class "report-postamble ui-helper-clearfix"]) ,(render-postamble seed start count total))))))
  
  ; seed integer integer integer -> xml
  (define/public (render-preamble seed start count total)
    (xml ,(opt-xml (get-show-controls?)
            (div (@ [id    ,(format "~a-controls" (get-id))]
                    [class "controls ui-helper-clearfix"])
                 ,(render-view-field seed)
                 ,(render-control-links seed start count total)
                 ,(render-pattern-field seed)))
         ,(opt-xml (get-show-pager-top?) 
            ,(render-pager    seed start count total))))
  
  ; seed integer integer integer -> xml
  (define/public (render-postamble seed start count total)
    (xml ,(opt-xml (get-show-pager-bottom?) 
            ,(render-pager    seed start count total))))
  
  ; seed -> xml
  (define/pubment (render-view-field seed)
    (send view-field set-visible?! (get-show-views?))
    (opt-xml (send view-field get-visible?)
      (div (@ [class "view"]) ,(inner (send view-field render seed) render-view-field seed))))
  
  ; seed -> xml
  (define/pubment (render-pattern-field seed)
    (send pattern-field set-visible?! (get-show-pattern-field?))
    (opt-xml (send pattern-field get-visible?) 
      (div (@ [class "filter"]) ,(inner (send pattern-field render seed) render-pattern-field seed))))
  
  ; seed integer integer integer -> xml
  (define/public (render-control-links seed start count total)
    (let ([links (get-control-links seed start count total)])
      (opt-xml (pair? links) 
        (div (@ [class "links"])
             (ul (@ [class "links"])
                 ,@(map (lambda (link) (xml (li ,link))) links))))))
  
  ; -> (listof xml)
  ; Each xml element should be an anchor (link).
  (define/public (get-control-links seed start count total)
    (assemble-list
     [(get-show-csv-link?)
      (xml (a (@ [href ,(embed/full seed (callback csv-download))]
                 [target "_new"])
              "CSV version"))]))
  
  ; seed integer integer integer -> xml
  (define/public (render-pager seed start count total)
    (send* pager-field 
      [init!  start count total]
      [render seed]))
  
  ; Main report rendering ----------------------
  
  ; seed integer integer integer (listof report-column%) (g:of any) -> xml
  (define/public (render-report seed start count total cols g:item)
    (xml (table (@ [id ,(get-table-id)] [class "snooze-report-table ui-widget"])
                ,(render-head seed cols)
                ,(if (zero? total)
                     (render-empty-body seed cols)
                     (render-body seed cols g:item))
                ,(render-foot seed cols))))
  
  ; seed (listof column) -> xml
  (define/public (render-head seed cols)
    ; column
    (define current-col (get-sort-col))
    ; (U 'asc 'desc)
    (define current-dir (get-sort-dir))
    ; xml
    (xml (thead (tr (@ [class 'ui-widget-header])
                    ,@(for/list ([col (in-list (get-visible-columns))])
                        (send col render-head seed (and (equal? col current-col) current-dir)))))))
  
  ; seed (listof column) -> xml
  (define/public (render-empty-body seed cols)
    (xml (tbody (@ [class "ui-widget-content"])
                (tr (td (@ [colspan ,(length cols)]
                           [class "empty-row"])
                        "There are no items to display in this list.")))))
  
  ; seed (listof column) (gen-> item) -> xml
  (define/public (render-body seed cols g:item)
    (xml (tbody (@ [class "ui-widget-content"])
                ,@(g:collect (g:map (cut render-item seed cols <>) g:item)))))
  
  ; seed (listof column) item -> xml
  (define/public (render-item seed cols item)
    (error "render-item must be overwritten"))
  
  ; seed (listof column) -> xml
  (define/public (render-foot seed cols)
    (xml))
  
  ; CSV ----------------------------------------
  
  ; -> (listof column)
  (define/public (get-csv-columns)
    (filter (cut send <> get-display-in-csv?)
            (get-all-columns)))
  
  ; string -> void
  (define/public #:callback (csv-download)
    (respond/csv))
  
  ; [string] -> string
  (define/public (get-csv-download-filename [prefix "download"])
    (format "~a-~a.csv" prefix (date->string (current-date) "~Y-~m-~d-~H-~M-~S")))
  
  ; [string] -> string
  (define/public (get-csv-download-headers [download-filename (get-csv-download-filename)])
    ; string
    (define content-disposition-string
      (format "attachment; filename=~a" download-filename))
    ; header
    (define content-disposition-header
      (make-header #"Content-Disposition" (string->bytes/utf-8 content-disposition-string)))
    ; header
    (define content-type-header
      (make-header #"Content-Type" #"text/csv"))
    (list* content-disposition-header
           content-type-header
           no-cache-http-headers))
  
  ; [(listof column)] -> void
  (define/public (respond/csv [cols (get-csv-columns)])
    ; void
    (send/suspend/dispatch
     (lambda (embed-url)
       (make-csv-response
        #:headers (get-csv-download-headers)
        (render/csv cols)))))
  
  ; [(listof column)] -> csv:sheet
  (define/public (render/csv [cols (get-csv-columns)])
    ; integer integer integer (gen-> row-data)
    (define-values (start count total g:item)
      (do-queries #:start #f #:count #f))
    (csv:sheet (render-head/csv cols)
               (render-body/csv cols g:item)
               (render-foot/csv cols)))
  
  ; (listo column) -> (treeof csv:row)
  (define/public (render-head/csv cols)
    (csv:row (map (cut send <> render-head/csv) cols)))
  
  ; (listof column) (gen-> item) -> (treeof csv:row)
  (define/public (render-body/csv cols g:item)
    (g:collect (g:map (cut render-item/csv cols <>) g:item)))
  
  ; (listof column) item -> (treeof csv:row)
  (define/public (render-item/csv cols item)
    (error "render-item/csv must be overridden."))
  
  ; (listof column) -> (treeof csv:row)
  (define/public (render-foot/csv cols)
    null)
  
  ; Attach and detach --------------------------
  
  ; seed -> js
  (define/augride (get-on-attach seed)
    (js ,@(filter-map 
           (lambda (col)
             (let ([sort-id (send col get-sort-id)]) ; (U symbol #f)
               ; Unsortable columns need no attaching:
               (and sort-id (js (!dot ($ ,(format "#~a" sort-id))
                                      (click (function ()
                                               ,(embed/ajax seed (callback on-sort (symbol->string (send col get-id)))))))
                                (!dot ($ ,(format "#~a .ui-state-default:not(.ui-state-disabled):not(.not-sortable)" 
                                                  (get-id)))
                                      (hover (function (event ui)
                                               (!dot ($ this) (addClass "ui-state-hover")))
                                             (function (event ui)
                                               (!dot ($ this) (removeClass "ui-state-hover")))))))))
           (get-visible-columns))))
  
  ; seed -> js
  (define/augride (get-on-detach seed)
    (js ,@(filter-map (lambda (col)
                        (let ([sort-id (send col get-sort-id)]) ; (U symbol #f)
                          ; Unsortable columns need no attaching:
                          (and sort-id (js (!dot ($ ,(format "#~a" sort-id))
                                                 (unbind))))))
                      (get-visible-columns)))))

; (U entity #f) -> symbol
(define (entity-report-id entity)
  (gensym/interned (symbol-append (if entity (entity-name entity) 'entity) '-report)))

; Provide statements --------------------------- 

(provide (except-out (all-from-out "report-column.ss"
                                   "report-view.ss"
                                   "report-pager.ss")
                     view)
         snooze-report%)

(provide/contract
 [default-show-csv-link? (parameter/c boolean?)]
 [entity-report-id       (-> (or/c entity? #f) symbol?)])