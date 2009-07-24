#lang scheme/base

(require scheme/contract
         (only-in scheme/list flatten)
         (only-in srfi/1 make-list)
         scheme/match
         scheme/string
         (planet untyped/snooze:3)
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn)
         (only-in (planet untyped/unlib:3/list) list-ref?)
         "core.ss"
         "read.ss"
         "generic-parse-types.ss")

; Procedures -------------------------------------

; Generic CSV parsing procedure. Parses a given CSV file using a multi-stage process,
; where some stages require user-specified data:
; [1] Read bytes into raw CSV lines
; [2] type-check: check the types of each row, where the type is specified as 
;     a list of csv-columns (and associated parse-types)
; [3] Duplication check: checks whether any lines contain duplicate information.
;     "Duplication" is defined by a list of keying-procedures, which encapsulate
;     a list of columns whose data, when taken together, form a key that must be unique.
;     Keying procedures are user-defined, and can be created using the
;     (make-key-generator ...) procedure from core.ss.
; [4] data validation and action creation: a user-defined procedure for error-checking,
;     validation, and construction of ensuing actions (e.g. save record).
;     This procedure needs to:
;     a) read the type-checked data
;     b) perform any custom validations
;     c) create an appropriate csv-action
;
;  bytes
;  (listof csv-column)
;  (csv-line/data -> (U csv-line/actions csv-line/errors)
;  [(listof key-generator)]
;  [#:rest-type parse-type?]
; ->
;  (listof (U csv-line/action csv-line/error))
(define (parse-csv file-content
                   type-specification
                   raw->data
                   [keying-procedures null]
                   #:rest-type [rest-type #f])    
  ; boolean
  (define trim-lines?
    (not rest-type))
  
  ; [1] Read the byte-string file contents into a list of csv-lines
  (define csv-lines
    (read-csv file-content (length type-specification) trim-lines?))
  
  ; [2] type-checking: iterate over all rows, applying the type-check
  ;     against the list of csv-columns
  ; (listof (U csv-line/data csv-line/errors))  
  (define typed-csv-lines
    (for/list ([csv-line (in-list csv-lines)])
      (csv-type-check csv-line type-specification rest-type)))
  
  ; [2] create a hashset for duplicate-checking
  ; hash: (keying-procedure) --> item
  (define hash:duplicates 
    (create-duplicate-hash typed-csv-lines type-specification keying-procedures))
  
  ; [3] apply duplicate removal, then custom filtering
  ; (listof (U csv-line/data csv-line/errors))
  (parse-csv-lines typed-csv-lines 
                   raw->data
                   type-specification
                   keying-procedures
                   hash:duplicates))

; A given CSV-line [Arg1] is type-checked against a sequence 
; of column-type specifications [Arg2]. Iterates through columns
; of CSV-line checking each against the corresponding column
; type specification.
;
;  csv-line/raw
;  (listof (integer . csv-column))
;  (U csv-column #f)
; ->
;  (U csv-line/data csv-line/errors)
(define (csv-type-check csv-line csv-columns rest-type)
  (match csv-line
    [(struct csv-line/raw (number text problems data))
     ; (listof (integer . csv-column)) - as many csv-columns as their are data elements in line
     (let ([csv-columns/rest (make-all-column-types data csv-columns rest-type)])
       (let-values ([(the-data the-problems)
                     (for/fold ([acc-data      null]
                                [acc-problems  null])
                       ([cell    (in-list data)]
                        [csv-col (in-list csv-columns/rest)]
                        [index   (in-naturals 1)]) 
                       (let-values ([(this-data this-problems)
                                     (csv-col cell index)])
                         (values (cons this-data acc-data)
                                 (check-problems this-problems acc-problems))))])
         ; convert to a csv-line/data or csv-line/errors
         (csv-line/raw->csv-line/data csv-line
                                      #:problems (reverse the-problems) #:data (reverse the-data))))]))


; compiles a list of line numbers in which a unique key is used
; (list should contain only a single number, if used correctly)

;  (listof (U csv-line/data csv-line/errors))
;  (listof csv-column)
;  (csv-line -> any)                         
; ->
;  hash: (any --> (listof integer))
(define (create-duplicate-hash csv-lines csv-columns keying-procedures)
  ; (listof (integer . csv-column))
  (define csv-columns/indices
    (add-column-indices csv-columns))
  
  ; hash: (keying-procedure csv-line) --> (listof integer)
  (define hash:duplicates (make-hash))
  
  ; collate list of line numbers for each (supposedly) unique key
  (for ([csv-line (in-list csv-lines)])
    (when (csv-line/data? csv-line)
      (for ([keying-procedure (in-list keying-procedures)])
        (let-values ([(this-key affected-cols) (keying-procedure csv-line csv-columns/indices)])
          (when this-key
            (let* ([this-line-num (csv-line-number csv-line)]
                   [line-numbers  (hash-ref hash:duplicates this-key null)])
              (hash-set! hash:duplicates this-key (cons this-line-num line-numbers))))))))
  
  ; return collated hashtable
  hash:duplicates)

;  (listof csv-line/raw) 
;  (listof csv-column)
;  (csv-line/raw -> (U csv-line/data csv-line/errors) - transform function, defining warnings/errors
;  (csv-line -> (values hash-key (listof integer)))
;  hash
; ->
;  (listof (U csv-line/data csv-line/errors))          
(define (parse-csv-lines csv-lines raw->data csv-columns keying-procedures hash:duplicates)
  
  ; (listof (integer . csv-column)) - columns with their within-line index
  (define csv-columns/indices
    (add-column-indices csv-columns))
  
  ; csv-line/data -> (U csv-line/data csv-line/errors)
  (define (check-duplicate csv-line)
    (copy-csv-line 
     csv-line
     #:problems
     (check-problems
      (flatten
       (for/list ([keying-procedure (in-list keying-procedures)])
         (let-values ([(this-key affected-columns) (keying-procedure csv-line csv-columns/indices)])
           (opt-check this-key
             (check/annotate ([ann:csv-columns affected-columns])
               (let* ([duplicate-lines (hash-ref hash:duplicates this-key)])
                 (opt-check (list-ref? duplicate-lines 1) ; if there's more than one element...
                   (check-fail 
                    (format "DUPLICATION: The key {~a} is present in several lines: ~a" this-key 
                            (string-join (reverse (map number->string duplicate-lines)) " ")))))))))))))
  
  ; check-duplicates, then apply custom checking procedure to result
  (for/list ([csv-line (in-list csv-lines)])
    (if (csv-line/data? csv-line)
        (let ([csv-duplicate (check-duplicate csv-line)])
          (if (csv-line/data? csv-duplicate) (raw->data csv-duplicate) csv-duplicate))
        csv-line)))


; string (listof (U csv-line/action csv-line/error)) -> void
; actions must achieve by side-effect
(define (run-csv-actions message csv-lines decisions call-with-transaction-wrapper)
  (run-csv-actions/epilogue message csv-lines decisions void call-with-transaction-wrapper))

;  string                                        ; commission message, for transaction wrapper
;  (listof (U csv-line/action csv-line/error))   ; the lines, either with actions or errors
;  (listof boolean)                              ; whether or not to perform an action
;  ( -> any)                                     ; anything executed after all line actions run
;  (( -> any) string -> any)                     ; transaction wrapper
; ->
;  void
; actions must achieve by side-effect
(define (run-csv-actions/epilogue message csv-lines decisions epilogue call-with-transaction-wrapper)
  (let ([csv-line-count (length csv-lines)]
        [decision-count (length decisions)])
    (if (= csv-line-count decision-count)
        (begin (call-with-transaction-wrapper
                ; commit changes                                                
                (lambda ()
                  (for ([csv-action (in-list csv-lines)]
                        [act?       (in-list decisions)])
                    (when (csv-line/action? csv-action)
                      ((csv-line/action-action csv-action) act?)))
                  (epilogue))                                                
                message)
               (void))
        (raise-exn exn:fail:contract "CSV lines and decision-list must be same length"))))





; Helpers ----------------------------------------

; (listof any) (listof csv-column) (U parse-type #f) -> (listof csv-column)
; parse-type is defined as (U string #f) integer -> any (listof check-results)
(define (make-all-column-types data col-types rest-type)
  (if rest-type
      (let* ([data-length (length data)]
             [cols-length (length col-types)]
             [rest-length (- data-length cols-length)])
        (append col-types
                (for/list ([index (in-range (add1 cols-length) (add1 data-length))])
                  (make-csv-column "" rest-type))))
      col-types))



; (listof csv-column) -> (listof (integer . csv-column))
(define (add-column-indices csv-columns)
  (for/list ([column (in-list csv-columns)] 
             [index  (in-naturals 1)])
    (cons index column)))


; Provides ---------------------------------------

(provide  ann:csv-columns
          (except-out (all-from-out "generic-parse-types.ss") opt-check))

(provide/contract 
 [parse-csv                (->* (bytes?                                     ; raw bytes
                                 (listof csv-column?)                       ; type specification
                                 (-> csv-line? csv-line?))                  ; line-validator and action generator
                                ((listof key-generator/c)                   ; duplication keys
                                 #:rest-type (or/c (parse-type/c) false/c)) ; parse-type for all remaining columns
                                (listof (or/c csv-line/action? csv-line/errors?)))]
 [run-csv-actions          (-> string?                                           ; message
                               (listof (or/c csv-line/action? csv-line/errors?)) ; csv-lines
                               (listof boolean?)                                 ; decisions as to whether to act
                               (-> (-> any) string? any)                         ; transaction wrapper
                               void?)]
 [run-csv-actions/epilogue (-> string?                                           ; message
                               (listof (or/c csv-line/action? csv-line/errors?)) ; csv-lines
                               (listof boolean?)                                 ; decisions as to whether to act
                               (-> void?)                                        ; thunk to do final database changes
                               (-> (-> any) string? any)                         ; transaction wrapper 
                               void?)])                              
