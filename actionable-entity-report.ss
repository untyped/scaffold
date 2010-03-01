#lang scheme/base

(require "base.ss")

(require (only-in (unlib-in list) list-ref?) 
         (unlib-in string symbol)
         "actionable-entity-report-internal.ss"
         "entity-report.ss"
         "report-column.ss"
         "report-internal.ss")

(define default-report-action
  (create-report-action 'default-report-action "With selected ..."))

; Classes ----------------------------------------

(define-class report-action-combo-box% vanilla-combo-box% ()
  (inherit get-id)
  
  ; Fields -------------------------------------
  
  ; actionable-entity-report%
  (init-field report #:accessor)
  
  ; Constructor --------------------------------
  
  (super-new)
  
  ; Methods ------------------------------------
  
  ; -> (listof any)
  (define/override (get-options)
    (send report get-report-actions))
  
  ; any -> (U string #f)
  (define/override (option->raw report-action)
    (report-action-id report-action))
  
  ; (U string #f) -> any
  (define/override (raw->option raw)
    (let ([raw-sym (and raw (string->symbol raw))])
      (for/or ([report-action (in-list (get-options))])
        (and (eq? raw-sym (report-action-id report-action)) report-action))))
  
  ; any -> string
  (define/override (option->xml report-action)
    (report-action-xml report-action))
  
  ; any -> string
  (define/override (option->classes report-action)
    (report-action-classes report-action))
  
  (define/override (option-enabled? report-action)
    (send report report-action-enabled? report-action))
  
  ; seed -> js
  (define/augment (get-on-attach seed)
    (js (!dot ($ ,(format "#~a" (get-id)))
              (change (function (event ui)
                        (if (!= (!dot ($ this) (val)) ,(report-action-id default-report-action))
                            (!block (var [dataObj (!object)])
                                    (!dot ($ ,(format "#~a input.report-action:checked" (send report get-id)))
                                          (each (function (elem index)
                                                  (= (!index dataObj (!dot ($ this) (attr "id"))) #t))))
                                    (!dot Smoke (doAjax ,(embed seed (callback [report on-run-report-action])) dataObj)))))))))
  
  ; seed -> js
  (define/augment (get-on-detach seed)
    (js (!dot ($ ,(format "#~a" (get-id))) (unbind)))))


; Interfaces -------------------------------------
(define actionable-entity-report<%>
  (interface ()
    get-report-actions            ; -> (listof report-action)
    report-action-enabled?        ; report-action -> boolean
    get-selected-items            ; (listof any) -> void
    set-selected-items!           ; -> (listof any)
    render-report-action-selector ; seed -> xml
    render-report-action-column   ; seed col any -> xml
    data->report-action-input-id  ; any -> (U symbol string)
    on-run-report-action          ; -> void
    run-report-action))           ; report-action -> void

; Columns ----------------------------------------

(define report-action-column
  (make-column 'report-action
               "Actions"
               #:xml-name         (xml (input (@ [type 'checkbox] [id 'toggle-all-selection])))
               #:classes          '(report-action)
               #:display-in-html? #t
               #:display-in-csv?  #f))

; entity-report subclass -------------------------

(define-class actionable-entity-report%  entity-report% (actionable-entity-report<%>)
  (inherit do-queries 
           get-entity 
           get-id
           get-show-pattern-field?
           get-show-pager-top?
           get-show-pager-bottom?
           get-sort-col
           get-sort-dir
           get-views
           get-visible-columns
           render-body
           render-pager
           render-controller-cell
           show-controller-cell?)
  
  ; Fields -------------------------------------
  
  ; (listof report-action)
  (init-field report-actions (error "A list of report-actions must be specified."))
  
  ; Child-components ---------------------------
  
  (field report-action-combo 
    (new report-action-combo-box% [report this])
    #:child #:accessor)
  
  ; Cells --------------------------------------
  ; (cellof (listof any))
  (cell selected-items null #:accessor #:mutator) 
  
  ; (cellof boolean)
  (cell dirty? #f #:accessor #:mutator)
  
  ; Constructor --------------------------------
  
  (init [classes null])
  (super-new [classes (list* "actionable-entity-report" classes)])
  
  ; Methods ------------------------------------
  
  ; -> boolean 
  (define/override (dirty?) 
    (begin0 (or (super dirty?) 
                (send report-action-combo dirty?)
                (get-dirty?))
            (set-dirty?! #f)))
  
  ; -> (listof any)
  (define/public (get-report-actions)
    (cons default-report-action report-actions))
  
  ; -> boolean
  (define/public (report-actions-enabled?)
    (pair? (get-report-actions)))
  
  ; -> (listof any)
  (define/public (get-report-action-column)
    report-action-column)
  
  ; -> boolean
  (define/public (report-action-enabled? report-action)
    #t)
  
  ; seed -> xml
  (define/public (render-report-action-selector seed)
    (xml (div (@ [class "report-actions"]) 
              ,(opt-xml (report-actions-enabled?)
                 ,(send report-action-combo render seed)))))
  
  ; Overridden rendering -----------------------
  
  ; seed -> xml
  (define/override (render-preamble seed start count total)
    (xml ,(super render-preamble seed start count total)
         ,(render-report-action-selector seed)))
  
  ; seed (listof column) -> xml
  (define/override (render-empty-body seed cols)
    (xml (tbody (tr (td (@ [colspan ,(+ (length cols)
                                        (if (show-controller-cell?)
                                            (if (report-actions-enabled?) 2 1)
                                            (if (report-actions-enabled?) 1 0)))]
                           [class "empty-row"])
                        "There are no items to display in this list.")))))
  
  ; seed (listof column) -> xml
  (define/override (render-head seed cols)
    (let ([current-col (get-sort-col)]
          [current-dir (get-sort-dir)])
      (xml (thead (tr (@ [class 'ui-widget-header])
                      ,(opt-xml (report-actions-enabled?)
                         ,(send report-action-column render-head seed #f))
                      ,(opt-xml (show-controller-cell?)
                         (th (@ [class "controller-cell ui-state-default"])
                             (& nbsp)))
                      ,@(for/list ([col (in-list (get-visible-columns))])
                          (send col render-head seed (and (equal? col current-col) current-dir))))))))
  
  ; seed (listof column) snooze-struct -> xml
  (define/overment (render-item-columns seed cols struct)
    (xml ,(render-report-action-column seed struct)
         ,(inner (super render-item-columns seed cols struct) render-item-columns seed cols struct)))
  
  ; seed snooze-struct -> xml
  (define/public (render-report-action-column seed struct)
    (let ([input-id (data->report-action-input-id struct)])
      (opt-xml (report-actions-enabled?)
        (td (@ [class "report-action-cell"])
            (input (@ [type 'checkbox]
                      [class "report-action"]
                      [id    ,input-id]
                      ,(opt-xml-attr (memq input-id (get-selected-items))
                                     checked 'checked)))))))
  
  (define/public (get-empty-selection-message)
    (xml "You must select one or more " ,(entity-pretty-name-plural (get-entity)) " to do that..."))
  
  ; any -> symbol
  (define/public (data->report-action-input-id struct)
    (string->symbol (format "report-action-~a-~a" (entity-name (snooze-struct-entity struct)) (snooze-struct-id struct))))
  
  ; any -> symbol
  (define/public (report-action-input-id->data id)
    (match (regexp-split #rx"-" (symbol->string id))
      [(list "report" "action" the-entity the-id)
       (and (string->number the-id) (find-by-id (get-entity) (string->number the-id)))]
      [_ #f]))
  
  ; Events -------------------------------------
  
  ; -> void
  (define/public-final #:callback (on-run-report-action)
    (let* ([report-action         (send report-action-combo get-value)]
           [selected-items (for/fold ([selected null])
                                     ([binding  (in-list (request-bindings (current-request)))])
                                     (match-let* ([(cons id val) binding]
                                                  [elem          (report-action-input-id->data id)])
                                       (if elem (cons elem selected) selected)))])
      (cond [(or (not report-action) (eq? report-action default-report-action))
             (void)]
            [(null? selected-items)
             (set-selected-items! (map (cut data->report-action-input-id <>) selected-items))
             (notifications-add! (get-empty-selection-message))
             (set-dirty?! #t)]
            [else
             (set-selected-items! (map (cut data->report-action-input-id <>) selected-items))
             (run-report-action report-action selected-items)
             (send report-action-combo set-value! default-report-action)
             (set-dirty?! #t)])))
  
  ; report-action -> void
  (define/public (run-report-action report-action selected-items)
    (error "run-report-action must be overridden"))
  
  ; JavaScript ---------------------------------
  ; seed -> js
  (define/override (get-on-attach seed)
    (js ,(super get-on-attach seed)
        ,(opt-js (report-actions-enabled?)
           (var [allCheckboxes ($ ,(format "#~a input.report-action" (get-id)))]
                [actionCombo   ($ ,(format "#~a" (send report-action-combo get-id)))]
                [enableCombo   (function ()
                                 (if (== (!dot ($ ,(format "#~a input.report-action:checked" (get-id))) (size)) 0)
                                     (!block (!dot actionCombo (attr "disabled" "disabled")))
                                     (!block (!dot actionCombo (removeAttr "disabled")))))])
           (!dot ($ "#toggle-all-selection")
                 (click (function (event ui)
                          (if (!dot ($ this) (attr "checked"))
                              (!block (!dot allCheckboxes (attr "checked" "checked")))
                              (!block (!dot allCheckboxes (removeAttr "checked"))))
                          (enableCombo))))
           (!dot allCheckboxes (click enableCombo))
           (enableCombo))))
  
  ; seed -> js
  (define/override (get-on-detach seed)
    (js ,(super get-on-detach seed)
        (!dot ($ "#select-all")  (unbind))
        (!dot ($ "#select-none") (unbind))
        (!dot ($ ,(format "#~a input.report-action" (get-id))) (unbind)))))

; Provide statements -----------------------------

(provide actionable-entity-report%)

(provide (except-out (all-from-out "actionable-entity-report-internal.ss") create-report-action make-report-action)
         (rename-out [create-report-action make-report-action])
         default-report-action)
