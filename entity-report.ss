#lang scheme/base

(require "base.ss")

(require (only-in srfi/13 string-fold-right string-trim-both)
         (only-in (unlib-in list) list-ref?) 
         (unlib-in string symbol)
         "controller-internal.ss"
         "report-column.ss"
         "report-internal.ss")

; Columns ----------------------------------------

(define attribute-report-column%
  (class/cells snooze-report-column% ()
    
    ; (listof attribute)
    (init-field attribute #:accessor)
    
    (init-field review-controller #f #:accessor)
    
    (init [id          (string->symbol
                        (format "~a-~a"
                                (entity-name (attribute-entity attribute))
                                (attribute-name attribute)))]
          [string-name (string-sentencecase (attribute-pretty-name attribute))]
          [order       (list (sql:order (sql:alias (entity-default-alias (attribute-entity attribute))
                                                   attribute)
                                        'asc))])
    
    (super-new [id          id]
               [string-name string-name]
               [order       order])
    
    ; seed any -> xml
    ; val is the raw attribute value - the report has to do all the destructuring.
    (define/public (render-body seed val)
      (xml (td ,(cond [(snooze-struct? val)
                       (cond [(get-review-controller)
                              => (lambda (controller)
                                   (xml (a (@ [href ,(controller-url controller val)])
                                           ,(format-snooze-struct val))))]
                             [(review-controller-set? val)
                              (xml (a (@ [href ,(review-controller-url val)])
                                      ,(format-snooze-struct val)))]
                             [else (format-snooze-struct val)])]
                      [(and (enum-type? (attribute-type attribute))
                            (enum-type-enum (attribute-type attribute)))
                       => (lambda (enum)
                            (enum-prettify enum val))]
                      [else val]))))
    
    ; any -> csv-cell
    (define/public (render-body/csv val)
      (csv:cell (cond [(snooze-struct? val)
                       (format-snooze-struct val)]
                      [(and (enum-type? (attribute-type attribute))
                            (enum-type-enum (attribute-type attribute)))
                       => (lambda (enum)
                            (enum-prettify enum val))]
                      [else val])))))

; attribute -> column
(define (default-attribute-column attr)
  ((attribute-column-defaults) attr))

; (parameter (attribute -> column))
(define attribute-column-defaults
  (make-parameter (lambda (attr) (new attribute-report-column% [attribute attr]))))

; Classes ----------------------------------------

(define entity-report%
  (class/cells snooze-report% ()
    
    (inherit get-sort-col
             get-sort-dir
             get-sort-order
             get-visible-columns) 
    
    ; Fields -------------------------------------
    
    ; Constructor --------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof attribute)
    (init [attributes (and entity (entity-data-attributes entity))])
    
    ; (cell boolean)
    (init-cell controller-cell? #t
               #:accessor show-controller-cell?
               #:mutator  set-show-controller-cell?!)
    
    ; (listof editor<%>)
    (init-field columns
      (or (and attributes (map default-attribute-column attributes))
          (error "entity-report constructor: insufficient arguments"))
      #:accessor)
    
    ; (cell (listof view)
    (init-field views 
      (list (make-view 'default "Default" columns))
      #:override-accessor)
    
    (init [show-pattern-field? #f])
    (init [sort-col            (car columns)])
    (init [classes             null])
    
    ; Constructor --------------------------------
    
    (super-new [show-pattern-field? show-pattern-field?]
               [sort-col            sort-col]
               [classes             (list* 'smoke-entity-report 'ui-widget classes)])
    
    ; Methods ------------------------------------
    
    ; -> sql
    (define/public (query-from)
      (let-sql ([entity (get-entity)]) (sql entity)))
    
    ; string -> natural
    (define/override (query-num-items pattern)
      (let-sql ([entity (get-entity)])
               (find-one (sql (select #:what  (count entity.guid)
                                      #:from  ,(query-from)
                                      #:where ,(make-where pattern))))))
    
    ; string column (U 'asc 'desc) natural natural -> (gen-> result)
    (define/override (query-items pattern col dir start count)
      (let-sql ([entity (get-entity)])
               (g:find (sql (select #:what   entity
                                    #:from   ,(query-from)
                                    #:where  ,(make-where pattern) 
                                    #:order  ,(get-sort-order col dir)
                                    #:offset ,start
                                    #:limit  ,count)))))
    
    ; pattern
    (define/public (make-where pattern)
      (let ([entity (get-entity)])
        (if pattern 
            (apply sql:or
                   (for/fold/reverse
                    ([accum null])
                    ([col   (in-list (get-visible-columns))])
                    (if (is-a? col attribute-report-column%)
                        (let* ([attr (send col get-attribute)]
                               [type (attribute-type attr)]
                               [ATTR (sql:alias (entity-default-alias entity) attr)])
                          (cond [(and (string->number pattern) (numeric-type? type))
                                 (cons (sql:= ATTR pattern) accum)]
                                [(character-type? type)
                                 (cons (sql:regexp-match-ci ATTR (pattern->regexp pattern)) accum)]
                                [else                   accum]))
                        accum)))
            (sql #t))))
    
    ; seed (listof column) -> xml
    (define/override (render-empty-body seed cols)
      (xml (tbody (tr (td (@ [colspan ,(if (show-controller-cell?)
                                           (add1 (length cols))
                                           (length cols))]
                             [class "empty-row"])
                          "There are no items to display in this list.")))))
    
    ; seed (listof column) -> xml
    (define/override (render-head seed cols)
      (let ([current-col (get-sort-col)]
            [current-dir (get-sort-dir)])
        (xml (thead (tr (@ [class 'ui-widget-header])
                        ,(opt-xml (show-controller-cell?)
                           (th (@ [class "controller-cell"])
                               (& nbsp)))
                        ,@(for/list ([col (in-list (get-visible-columns))])
                            (send col render-head seed (and (equal? col current-col) current-dir))))))))
    
    ; seed (listof column) snooze-struct -> xml
    (define/override (render-item seed cols struct)
      (xml (tr ,(render-item-columns seed cols struct))))
    
    ; seed (listof column) snooze-struct -> xml
    (define/public (render-item-columns seed cols struct)
      (xml ,(opt-xml (show-controller-cell?)
              ,(render-controller-cell seed struct))
           ,@(for/list ([col (in-list cols)])
               (render-column seed col struct))))
    
    ; seed column snooze-struct -> xml
    (define/public (render-column seed col struct)
      (if (is-a? col attribute-report-column%)
          (let ([attr (send col get-attribute)])
            (send col render-body seed (snooze-struct-ref struct attr)))
          (error "entity-report.render-column: could not render column" col)))
    
    ; entity -> string
    (define (entity-csv-prefix str)
      (regexp-replace* #rx"[ \t\r\n]+"
                       (string-trim-both (entity-pretty-name-plural entity))
                       "-"))
    
    ; [string] -> string
    (define/override (get-csv-download-filename
                      [prefix (cond [(get-entity) => entity-csv-prefix]
                                    [else            "download"])])
      (super get-csv-download-filename prefix))
    
    ; (listof column) snooze-struct -> csv-row
    (define/override (render-item/csv cols struct)
      (apply csv:row (for/list ([col (in-list cols)])
                       (render-column/csv col struct))))
    
    ; column snooze-struct -> csv-cell
    (define/public (render-column/csv col struct)
      (if (is-a? col attribute-report-column%)
          (let ([attr (send col get-attribute)])
            (send col render-body/csv (snooze-struct-ref struct attr)))
          (error "entity-report.render-column: could not render column" col)))
    
    ; seed string -> xml
    (define/public (render-controller-cell seed struct)
      (xml (td (@ [class "controller-cell"])
               ,(opt-xml (review-controller-set? struct)
                  ,(controller-link
                    (review-controller-ref struct)
                    struct
                    #:body (xml (div (@ [class "controller-icon ui-state-default ui-corner-all"]
                                        [title "View this item"])
                                     (!icon (@ [type "search"]))))
                    #:else (xml (div (@ [class "controller-icon ui-state-disabled ui-corner-all"]
                                        [title "Cannot view this item"])
                                     (!icon (@ [type "search"]))))))
               ,(opt-xml (update-controller-set? struct)
                  ,(controller-link
                    (update-controller-ref struct)
                    struct
                    #:body (xml (div (@ [class "controller-icon ui-state-default ui-corner-all"]
                                        [title "Edit this item"])
                                     (!icon (@ [type "pencil"]))))
                    #:else (xml (div (@ [class "controller-icon ui-state-disabled ui-corner-all"]
                                        [title "Cannot edit this item"])
                                     (!icon (@ [type "pencil"]))))))
               ,(opt-xml (delete-controller-set? struct)
                  ,(controller-link
                    (delete-controller-ref struct)
                    struct
                    #:body (xml (div (@ [class "controller-icon ui-state-default ui-corner-all"]
                                        [title "Delete this item"])
                                     (!icon (@ [type "trash"]))))
                    #:else (xml (div (@ [class "controller-icon ui-state-disabled ui-corner-all"]
                                        [title "Cannot delete this item"])
                                     (!icon (@ [type "trash"])))))))))
    
    ; string [boolean] -> string
    (define/public (pattern->regexp pattern [anywhere? #f])
      (apply string-append
             (cons (if anywhere? "^.*" "^")
                   (string-fold-right (lambda (chr accum)
                                        (cond [(eq? chr #\*) (cons ".*" accum)]
                                              [(eq? chr #\?) (cons "." accum)]
                                              [else          (cons (regexp-quote (string chr)) accum)]))
                                      null
                                      pattern))))))

(define default-action-enum-column
  (make-parameter (make-column 'action-enum "")))

(define actionable-entity-report<%>
  (interface ()
    get-action-enum           ; -> enum
    actions-enabled?          ; -> boolean
    get-selected-items        ; (listof any) -> void
    set-selected-items!       ; -> (listof any)
    render-action-selector    ; seed -> xml
    render-action-enum-column ; seed col any -> xml
    data->action-input-id       ; any -> (U symbol string)
    on-run-action             ; -> void
    run-action))              ; (enum-value/c action-enum) -> void

(define actionable-entity-report%
  (class/cells entity-report% (actionable-entity-report<%>)
    (inherit do-queries 
             get-entity 
             get-views
             get-show-pattern-field?
             get-id
             render-control-links
             render-controller-cell
             show-controller-cell?)
    (inherit-field view-field pattern-field)
    
    ; Child-components ---------------------------
    
    (field action-combo (new combo-box% [visible? #f] [on-change (callback on-run-action)]) #:child #:accessor)
    
    (field action-enum-column (default-action-enum-column))
    
    ; Fields -------------------------------------
    
    ; (U enum #f)
    (init-field action-enum #f #:accessor)
    
    ; Cells --------------------------------------
    ; (cellof (listof any))
    (cell selected-items null #:accessor #:mutator) 
    
    ; Constructor --------------------------------
    
    (super-new)
    (when action-enum
      (send* action-combo 
        [set-visible?! #t]
        [set-options!  (map (lambda (val)
                              (cons val (enum-prettify action-enum val)))
                            (enum-values action-enum))]))
    
    ; Methods ------------------------------------
    
    ; -> boolean
    (define/public (actions-enabled?)
      (and action-enum #t))
    
    ; seed -> xml
    (define/public (render-action-selector seed)
      (send action-combo render seed))
    
    ; Overridden rendering -----------------------
    
    ; -> (listof column)
    (define/override (get-visible-columns)
      (let ([cols (super get-visible-columns)])
        (if (actions-enabled?) (append cols (list action-enum-column)) cols)))
    
    ; seed integer integer integer -> xml
    (define/override (render-controls seed start count total)
      (let ([show-view-field? (list-ref? (get-views) 1)]) ; multiple views
        (send view-field    set-visible?! show-view-field?)
        (send pattern-field set-visible?! (get-show-pattern-field?))
        (xml (div (@ [id    ,(format "~a-controls" (get-id))]
                     [class "controls ui-helper-clearfix"])
                  (div (@ [class "view"])
                       ,(send view-field render seed))
                  ,(opt-xml (actions-enabled?)
                     (div (@ [class "actions"])
                          ,(render-action-selector seed)))
                  ; extra link controls:
                  (div (@ [class "links"])
                       ,(render-control-links seed start count total))
                  ; Filter pattern field is always visible:
                  (div (@ [class "filter"])
                       ,(send pattern-field render seed))))))
    
    ; seed (listof column) snooze-struct -> xml
    (define/override (render-item-columns seed cols struct)
      (xml ,(opt-xml (show-controller-cell?)
              ,(render-controller-cell seed struct))
           ,@(for/list ([col (in-list cols)])
               (render-column seed col struct))))
    
    ; seed column snooze-struct -> xml
    (define/overment (render-column seed col struct)
      (if (eq? col action-enum-column)
          (render-action-enum-column seed col struct)
          (inner (super render-column seed col struct) render-column seed col struct)))
    
    ; seed column snooze-struct -> xml
    (define/public (render-action-enum-column seed col struct)
      (xml (td (input (@ [type 'checkbox]
                         [class "action-enum"]
                         [id    ,(data->action-input-id struct)])))))
    
    (define/public (get-empty-selection-message)
      (xml "You must select one or more " ,(entity-pretty-name-plural (get-entity)) " to do that..."))
    
    ; any -> symbol
    (define/public (data->action-input-id struct)
      (string->symbol (format "action-enum-~a-~a" (entity-name (snooze-struct-entity struct)) (snooze-struct-id struct))))
    
    ; any -> symbol
    (define/public (action-input-id->data id)
      (match (regexp-split #rx"-" (symbol->string id))
        [(list "action" "enum" the-entity the-id)
         (and (string->number the-id) (find-by-id (get-entity) (string->number the-id)))]
        [_ #f]))
    
    ; Events -------------------------------------
    
    ; -> void
    (define/public-final #:callback (on-run-action)
      (let ([action         (send action-combo get-value)]
            [selected-items (get-selected-items)])
        (if (pair? selected-items)
            (run-action action selected-items)
            (notifications-add! (get-empty-selection-message)))))
    
    ; (enum-value/c action-enum) -> void
    (define/public (run-action action selected-items)
      (error "run-action must be overridden"))
    
    ; request -> void
    (define/augment (on-request req)
      (when (actions-enabled?)
        (set-selected-items!
         (reverse
          (for/fold ([selected null])
                    ([binding  (in-list (request-bindings req))])
                    (match-let* ([(cons id val) binding]
                                 [elem          (action-input-id->data id)])
                      (if elem (cons elem selected) selected)))))))
    
    (define/override (get-on-attach seed)
      (js ,(super get-on-attach seed)
          (!dot ($ ,(format "#~a .action-enum" (get-id)))
                (click (function (event ui)
                         (var [elem ($ this)]
                              [isSelected (!dot elem (attr "checked"))])
                         (if isSelected 
                             (!block (!dot Smoke (setSubmitData (!dot elem (attr "id")) #t)))
                             (!block (!dot Smoke (removeSubmitData (!dot elem (attr "id")))))))))))
    
    (define/override (get-on-detach seed)
      (js ,(super get-on-detach seed)
          (!dot ($ ,(format "#~a input.action-enum" (get-id)))
                (unbind))))))

; Provide statements -----------------------------

(provide entity-report%
         attribute-report-column%
         actionable-entity-report%)

(provide/contract
 [default-attribute-column  (-> attribute? (is-a?/c attribute-report-column%))]
 [attribute-column-defaults (parameter/c (-> attribute? (is-a?/c attribute-report-column%)))])
