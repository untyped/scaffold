#lang scheme/base

(require "base.ss")

(require (only-in srfi/13 string-fold-right string-trim-both)
         (only-in (unlib-in list) list-ref?) 
         (unlib-in string symbol)
         "controller-internal.ss"
         "report-column.ss"
         "report-internal.ss"
         "view-common.ss")

; Columns ----------------------------------------

(define-class attribute-report-column% snooze-report-column% ()
  
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
    (xml (td ,val)))
  
  ; any -> csv-cell
  (define/public (render-body/csv val)
    (csv:cell val)))

; attribute -> column
(define (default-attribute-column attr)
  ((attribute-column-defaults) attr))

; (parameter (attribute -> column))
(define attribute-column-defaults
  (make-parameter (lambda (attr) (new attribute-report-column% [attribute attr]))))

; Classes ----------------------------------------

(define-class entity-report% snooze-report% ()
  
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
          (xml (td ,(snooze-struct-xml-ref struct attr))))
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
                                    pattern)))))

; Provide statements -----------------------------

(provide entity-report%
         attribute-report-column%)

(provide/contract
 [default-attribute-column  (-> attribute? (is-a?/c attribute-report-column%))]
 [attribute-column-defaults (parameter/c (-> attribute? (is-a?/c attribute-report-column%)))])
