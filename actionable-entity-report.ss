#lang scheme/base

(require "base.ss")

(require (only-in (unlib-in list) list-ref?) 
         (unlib-in string symbol)
         "entity-report.ss"
         "report-column.ss"
         "report-internal.ss")

; Columns ----------------------------------------

(define default-action-enum-column
  (make-parameter (make-column 'action-enum "")))

; Interfaces -------------------------------------
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

; Classes ----------------------------------------

(define action-enum-combo-box%
  (class/cells combo-box% ()
    (inherit get-id)
    
    ; Fields -------------------------------------
    
    ; actionable-entity-report%
    (init-field report #:accessor)
    ; enum
    (init-field action-enum #:accessor)
    
    ; Constructor --------------------------------
    
    (super-new [options (cons (cons #f "-- Select action --")
                              (map (lambda (val)
                                     (cons val (enum-prettify action-enum val)))
                                   (enum-values action-enum)))])
    
    ; Methods ------------------------------------
    
    ; seed -> js
    (define/augment (get-on-attach seed)
      (js (!dot ($ ,(format "#~a" (get-id)))
                (change (function (event ui)
                          (var [dataObj (!object)])
                          (!dot ($ ,(format "#~a input.action-enum:checked" (send report get-id)))
                                (each (function (elem index)
                                        (= (!index dataObj (!dot ($ this) (attr "id"))) #t))))
                          (!dot Smoke (doAjax ,(embed seed (callback [report on-run-action])) dataObj)))))))
    
    ; seed -> js
    (define/augment (get-on-detach seed)
      (js (!dot ($ ,(format "#~a" (get-id))) (unbind))))))

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
    
    ; Fields -------------------------------------
    
    ; (U enum #f)
    (init-field action-enum #:accessor)
    
    ; Child-components ---------------------------
    
    (field action-combo 
      (new action-enum-combo-box% [report this] [action-enum action-enum])
      #:child #:accessor)
    
    (field action-enum-column (default-action-enum-column))
    
    
    ; Cells --------------------------------------
    ; (cellof (listof any))
    (cell selected-items null #:accessor #:mutator) 
    
    ; (cellof boolean)
    (cell dirty? #f #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> boolean 
    (define/override (dirty?) 
      (begin0 (or (super dirty?) 
                  (send action-combo dirty?)
                  (get-dirty?))
              (set-dirty?! #f)))
    
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
      (let ([input-id (data->action-input-id struct)])
        (xml (td (input (@ [type 'checkbox]
                           [class "action-enum"]
                           [id    ,input-id]
                           ,(opt-xml-attr (memq input-id (get-selected-items))
                                          checked 'checked)))))))
    
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
      (let* ([action         (send action-combo get-value)]
             [selected-items (for/fold ([selected null])
                                       ([binding  (in-list (request-bindings (current-request)))])
                                       (match-let* ([(cons id val) binding]
                                                    [elem          (action-input-id->data id)])
                                         (if elem (cons elem selected) selected)))])
        (cond [(not action)           (void)]
              [(null? selected-items) (set-selected-items! (map (cut data->action-input-id <>) selected-items))
                                      (notifications-add! (get-empty-selection-message))
                                      (set-dirty?! #t)]
              [else                   (set-selected-items! (map (cut data->action-input-id <>) selected-items))
                                      (run-action action selected-items)
                                      (send action-combo set-value! #f)
                                      (set-dirty?! #t)])))
    
    ; (enum-value/c action-enum) -> void
    (define/public (run-action action selected-items)
      (error "run-action must be overridden"))))

; Provide statements -----------------------------

(provide actionable-entity-report%)

(provide/contract
 [default-action-enum-column (parameter/c (-> (is-a?/c snooze-report-column%)))])
