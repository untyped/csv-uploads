#lang scheme/base

(require scheme/contract
         scheme/match
         (only-in srfi/13 string-trim-both)         
         (planet untyped/unlib:3/exn)
         (planet untyped/unlib:3/number)
         (planet untyped/snooze:2/check/check)
         (planet untyped/mirrors:1))

; Contracts --------------------------------------

(define (parse-type-criteria/c [return-type/c any/c])
  (-> (or/c string? false/c) return-type/c))

(define (parse-type/c [return-type/c any/c])
  (and/c parse-type? 
         (->* ((or/c string? false/c))
              (integer? csv-column?)
              (values return-type/c (listof check-result?)))))

(define key-generator/c 
  (-> csv-line/data?
      (listof (cons/c integer? csv-column?))
      (values (or/c string? false/c)
              (listof csv-column?))))

; Annotations ------------------------------------

; ann:csv-columns - columns, for when a CSV column contains incorrect data
(define-annotation ann:csv-columns
  (lambda (result) null)
  (lambda (result old new) (append old new)))

; ann:csv-unchanged - csv-line for when a CSV line is consistent with database
(define-annotation ann:csv-unchanged
  (lambda (result) #f)
  (lambda (result old new) new))

; Lines ------------------------------------------

; (struct integer string (listof check-problem)))
(define-struct csv-line (number text problems) #:transparent)

; (struct ... (listof string))
; where data is padded to the minimum number of columns.
(define-struct (csv-line/raw csv-line) (data) #:transparent)

; (struct ... (listof scheme-datum))
; where data is padded to the minimum number of columns.
; csv-line/data instances are allowed warnings, or no problems
(define-struct (csv-line/data csv-line) (data) #:transparent)

; (struct ... (listof string))
; no data included, just errors
; and csv-line with failures or errors is converted into a csv-line/errors
(define-struct (csv-line/errors csv-line) () #:transparent)

; (struct ... action)
(define-struct (csv-line/action csv-line/data) (action) #:transparent)

; (struct ... (listoc import-result))
(define-struct (csv-line/results csv-line/data) (results) #:transparent)

; Actions ----------------------------------------

; (struct xml)
(define-struct action (description procedure) #:transparent
  #:property prop:procedure (struct-field-index procedure))

; (struct xml (any -> import-result))
(define-struct (import-action action) () #:transparent)

; Results ----------------------------------------

; (struct xml xml)
(define-struct import-result (action-description description) #:transparent)

; (struct ...)
(define-struct (import-success import-result) () #:transparent)

; (struct ...)
(define-struct (import-warning import-result) () #:transparent)

; (struct ...)
(define-struct (import-failure import-result) () #:transparent)


; CSV columns ------------------------------------

; (struct string ((U string #f) -> any))
(define-struct csv-column (title parser) 
  #:transparent
  #:property 
  prop:procedure 
  (lambda (col raw index)
    ((csv-column-parser col) raw index col)))

; Parse types ------------------------------------

; DEFINING A PARSE TYPE:
; Parse-type is a wrapper struct for a parsing procedure, stored in parse-type-criteria.
; The "criteria" is a procedure taking a raw value (U string #f) and either:
; - If criteria are met, return the parsed value (any)
; - Otherwise, raise an exn:fail:csv:parse-type, with the message describing the problem.
;
; A parse-type instance may be invoked as a procedure with the following contract:
;  (U string #f)
;  [index]
;  [csv-column]
; -> 
;  (values any (listof check-result))
;
; If there are no parsing problems, "any" will be a parsed value,
; and "(listof check-result)" will be null.
;
; If there are no parsing problems, "any" will be a parsed value,
; and "(listof check-result)" will be null.

; (struct ((U string #f) -> any))
(define-struct parse-type (description criteria) #:transparent
  #:property prop:procedure
  (lambda (pt raw [index 1] [column (make-csv-column "internal column" pt)])    
    (let ([trimmed-raw (and raw (string-trim-both raw))])
      (with-handlers
          ([exn:fail:csv:parse-type?
            (lambda (exn)
              (values (void)
                      (check/annotate ([ann:csv-columns (list column)])
                        (check-fail (format "Column ~a (~a): Expected ~a; found ~s."
                                            index
                                            (csv-column-title column)
                                            (parse-type-description pt)
                                            (if (equal? trimmed-raw "") '<blank> trimmed-raw))))))])
        (values ((parse-type-criteria pt) trimmed-raw) null)))))

; -> exn:fail:csv:parse-type
(define (parse-fail)
  (raise-exn exn:fail:csv:parse-type "Parse-type violation"))

; Exceptions -------------------------------------

; (struct string continuation-mark-set) ...
(define-struct (exn:fail:csv exn:fail) () #:transparent)
(define-struct (exn:fail:csv:parse-type exn:fail:csv) () #:transparent)

; Constructors -----------------------------------

;  csv-line/data
;  [#:problems (listof check-result)]
;  [#:data (listof scheme-datum)]
; ->
;  (U csv-line/data csv-line/errors)
(define (copy-csv-line csv-line/data #:problems [problems null] #:data [data #f])
  (copy-csv-line-internal csv-line/data csv-line/data-data problems data))


;  csv-line/data
;  #:action   action
;  [#:problems (listof check-result)]
; ->
;  (U csv-line/action csv-line/errors)
(define (copy-csv-line/action csv-line/data #:action action #:problems [problems null])
  (let ([line (copy-csv-line csv-line/data #:problems problems)])
    (if (csv-line/data? line)
        (csv-line/data->csv-line/action line action)
        line)))

;  csv-line/raw
;  [#:problems (listof check-result)]
;  [#:data (listof scheme-datum)]
; ->
;  (U csv-line/data csv-line/errors)
(define (csv-line/raw->csv-line/data csv-line/raw #:problems [problems null] #:data [data #f])
  (copy-csv-line-internal csv-line/raw csv-line/raw-data problems data))

;  csv-line
;  (csv-line -> (listof scheme-datum))
;  (listof check-result)
;  (U (listof scheme-datum) #f)
; ->
;  (U csv-line/data csv-line/errors)
(define (copy-csv-line-internal csv-line data-accessor problems data)
  (let* ([all-problems (check-problems (csv-line-problems csv-line) problems)]
         [all-errors   (check-errors all-problems)]
         [line-number  (csv-line-number csv-line)]
         [text         (csv-line-text csv-line)]
         [data         (if data data (data-accessor csv-line))])
    (if (null? all-errors)
        (make-csv-line/data line-number text all-problems data)
        (make-csv-line/errors line-number text all-problems))))

;  csv-line/data
;  action
; ->
;  csv-line/action
(define (csv-line/data->csv-line/action csv-line/data action)
  (make-csv-line/action (csv-line-number csv-line/data)
                        (csv-line-text csv-line/data)
                        (csv-line-problems csv-line/data)
                        (csv-line/data-data csv-line/data)
                        action))


; Unique-key generators --------------------------

;  (listof csv-columns)
;  [#:include-false-values? boolean] - default #t. If #f, when all vals are #f, ignore.
; -> 
;  (csv-line/data 
;   (listof (integer . csv-column))
;  ->
;   (values (U string #f) (listof csv-column))))
(define (make-key-generator csv-columns #:include-false-values? [include-false-values? #t])
  (lambda (line-in-csv all-columns/indices)
    (match line-in-csv
      [(struct csv-line/data (_ _ _ data-list))
       (let-values 
           ([(columns all-values-false? value-string)
             (for/fold ([selected-columns  null] ; (listof csv-column)
                        [all-values-false? #t]   ; boolean, #t if all values seen so far are #f
                        [value-string      ""])  ; string
               ([index+col (in-list all-columns/indices)])               
               (let* ([index     (car index+col)] ; integer
                      [csv-col   (cdr index+col)]
                      [selected? (and (member csv-col csv-columns) #t)]
                      [the-data  (and selected? (list-ref data-list (sub1 index)))])
                 (if selected?
                     (values (cons csv-col selected-columns)
                             (and all-values-false? (not the-data))
                             (string-append value-string
                                            (format "Column ~a (~a)=~s; " 
                                                    index (csv-column-title csv-col) the-data)))
                     (values selected-columns all-values-false? value-string))))])                              
         (values (and (or include-false-values? (not all-values-false?)) value-string)
                 (reverse columns)))] ; reverse columns to get correct ordering
      [_ (raise-exn exn:fail:csv
                    (string-append (format "CSV lines must contain at least:~n")
                                   (map (lambda (col) (format " - ~s~n" (csv-column-title col)))
                                        csv-columns)
                                   "This information is not present in the current line"))])))



; Provide statements -----------------------------

(provide/contract
 [parse-type-criteria/c (->* () (contract?) contract?)]
 [parse-type/c          (->* () (contract?) contract?)]
 [key-generator/c       contract?])

(provide ann:csv-columns ann:csv-unchanged parse-fail)



; Structs
(provide/contract
 ; Lines:
 [struct csv-line                         ([number   natural?]
                                           [text     string?]
                                           [problems (listof check-problem?)])]
 [struct (csv-line/raw csv-line)          ([number   natural?]
                                           [text     string?]
                                           [problems (listof check-problem?)]
                                           [data     (listof string?)])] 
 [struct (csv-line/data csv-line)         ([number   natural?]
                                           [text     string?]
                                           [problems (listof check-problem?)]
                                           [data     list?])] 
 [struct (csv-line/errors csv-line)       ([number   natural?]
                                           [text     string?]
                                           [problems (listof check-problem?)])] 
 [struct (csv-line/action csv-line/data)  ([number   natural?]
                                           [text     string?]
                                           [problems (listof check-problem?)]
                                           [data     list?]
                                           [action   action?])]
 [struct (csv-line/results csv-line/data) ([number   natural?]
                                           [text     string?]
                                           [problems (listof check-problem?)]
                                           [data     list?]
                                           [results  import-result?])]
 ; Actions
 [struct action                           ([description        xml?]
                                           [procedure          (-> boolean? import-result?)])]
 [struct (import-action action)           ([description        xml?]
                                           [procedure          (-> boolean? import-result?)])]
 ; Results
 [struct import-result                    ([action-description xml?]
                                           [description        xml?])]
 [struct (import-success import-result)   ([action-description xml?]
                                           [description        xml?])]
 [struct (import-warning import-result)   ([action-description xml?]
                                           [description        xml?])]
 [struct (import-failure import-result)   ([action-description xml?]
                                           [description        xml?])]
 ; CSV column
 [struct csv-column                       ([title  string?]
                                           [parser any/c])]
 ; parse-types
 [struct parse-type                       ([description string?]
                                           [criteria    parse-type-criteria/c])]
 ; exceptions
 [struct exn:fail:csv                     ([message            string?]
                                           [continuation-marks continuation-mark-set?])])

; procedures
(provide/contract
 [csv-line/raw->csv-line/data    (->* (csv-line/raw?) 
                                      (#:problems (listof check-problem?) #:data list?)
                                      (or/c csv-line/data? csv-line/errors?))]
 [copy-csv-line/action           (->* (csv-line/data? #:action action?) 
                                      (#:problems (listof check-problem?))
                                      (or/c csv-line/data? csv-line/errors?))]
 [copy-csv-line                  (->* (csv-line/data?) 
                                      (#:problems (listof check-problem?) #:data list?)
                                      (or/c csv-line/data? csv-line/errors?))]
 [csv-line/data->csv-line/action (-> csv-line/data? action? csv-line/action?)]
 [make-key-generator             (->* ((listof csv-column?))
                                      (#:include-false-values? boolean?)                                         
                                      key-generator/c)])