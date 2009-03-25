#lang scheme/base

(require (for-syntax scheme/base)         
         scheme/contract
         scheme/string
         (only-in srfi/13 string-trim-both)
         (planet untyped/snooze:2/annotation)
         (planet untyped/snooze:2/check/check)
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/enum)
         (planet untyped/unlib:3/exn)
         "core.ss")

; Syntax -----------------------------------------

; (_ test check-expr)
(define-syntax (opt-check stx)
  (syntax-case stx ()
    [(_ test check-expr)
     #'(if test
           check-expr
           (check-pass))]))



; PARSE TYPE INSTANCES ---------------------------

; [1a] Strings -----------------------------------

(define parse-type:string 
  (make-parse-type "non-empty string"
    (lambda (raw)
      (let ([trimmed-raw (and raw (string-trim-both raw))])
        (if (and trimmed-raw (not (equal? trimmed-raw "")))
            (string-trim-both raw)
            (parse-fail))))))


; [1b] String + false ----------------------------

(define parse-type:string+false
  (make-parse-type "string or <blank>"
    (lambda (raw)
      (if (and raw (not (equal? raw "")))
          ((parse-type-criteria parse-type:string) raw)
          #f))))

; [2a] symbols -----------------------------------

; (U string #f) integer -> (U symbol void) (listof check-results)
(define (parse-type:symbol/case case-sensitive?)  
  (make-parse-type (if case-sensitive? "symbol" "symbol (case-insensitive)")
    (lambda (raw) 
      (let* ([trimmed-raw (and raw (string-trim-both (if case-sensitive? raw (string-downcase raw))))]
             [sym-val     (and trimmed-raw (not (equal? trimmed-raw "")) (string->symbol trimmed-raw))])
        (if (and sym-val (symbol? sym-val))
            sym-val
            (parse-fail))))))

(define parse-type:symbol
  (parse-type:symbol/case #t))

(define parse-type:symbol-ci
  (parse-type:symbol/case #f))


; [2b] Symbol + false ----------------------------
; (U string #f) integer string boolean -> (U symbol void #f) (listof check-results)
(define (parse-type:symbol+false/case case-sensitive?)
  (make-parse-type (if case-sensitive? "symbol or <blank>" "symbol (case-insensitive) or <blank>")
    (lambda (raw)
      (if (and raw (not (equal? raw "")))
          ((parse-type-criteria (if case-sensitive? parse-type:symbol parse-type:symbol-ci)) raw)
          #f))))

; (U string #f) csv-column -> (U symbol void #f) (listof check-results)
(define parse-type:symbol+false
  (parse-type:symbol+false/case #t))

; (U string #f) csv-column -> (U symbol void #f) (listof check-results)
(define parse-type:symbol-ci+false
  (parse-type:symbol+false/case #f))

; Parse-types: numbers ---------------------------

; (U string #f) csv-column -> (U number #f) (listof check-results)
(define parse-type:number+false
  (make-parse-type "number or <blank>"
    (lambda (raw)
      (if (and raw (not (equal? raw "")))
          ((parse-type-criteria parse-type:number) raw)
          #f))))

; (U string #f) integer -> number (listof check-results)
(define parse-type:number
  (make-parse-type "number"
    (lambda (raw)
      (let ([num-val (and raw (not (equal? raw "")) (string->number raw))])
        (if (number? num-val)
            num-val
            (parse-fail))))))

; (U string #f) integer -> integer (listof check-results)
(define parse-type:integer
  (make-parse-type "integer"
    (lambda (raw)
      (let-values ([(numeric-value _) (parse-type:number raw)])
        (if (integer? numeric-value)
            numeric-value
            (parse-fail))))))

; (U string #f) integer -> (U integer #f) (listof check-results)
(define parse-type:integer+false
  (make-parse-type "integer or <blank>"
    (lambda (raw)
      (if (and raw (not (equal? raw "")))
          ((parse-type-criteria parse-type:integer) raw)
          #f))))

; (U string #f) csv-column -> (U real void) (listof check-results)
(define parse-type:real
  (make-parse-type "real or <blank>"
    (lambda (raw)
      (let-values ([(numeric-value _) (parse-type:number raw)])
        (if (real? numeric-value)
            numeric-value
            (parse-fail))))))

; (U string #f) csv-column -> (U real void #f) (listof check-results)
(define parse-type:real+false  
  (make-parse-type "real or <blank>"
    (lambda (raw)
      (if (and raw (not (equal? raw "")))
          ((parse-type-criteria parse-type:real) raw)
          #f))))


; Booleans ---------------------------------------

; (U string #f) csv-column -> (U boolean void) (listof check-results)
(define parse-type:boolean
  (make-parse-type "boolean"
    (lambda (raw)
      (cond [(or (string-ci=? raw "true") (string-ci=? raw "yes") (string-ci=? raw "y"))
             #t]
            [(or (string-ci=? raw "false") (string-ci=? raw "no") (string-ci=? raw "n"))
             #f]
            [else (parse-fail)]))))


; (U string #f) csv-column -> (U boolean #f) (listof check-results)
; unspecified -> false
(define parse-type:boolean+unspecified  
  (make-parse-type "boolean or <blank>"
    (lambda (raw)
      (if (and raw (not (equal? raw "")))
          ((parse-type-criteria parse-type:boolean) raw)
          #f))))


; MD5 --------------------------------------------

(define parse-type:md5
  (make-parse-type "unique identifier"
    (lambda (raw)
      (if (regexp-match #px"^[0-9a-f]{32}$" raw)
          raw
          (parse-fail)))))

(define parse-type:md5+false
  (make-parse-type "unique identifier or <blank>"
    (lambda (raw)
      (if (and raw (not (equal? raw "")))
          ((parse-type-criteria parse-type:md5) raw)
          #f))))

; Compounds --------------------------------------

; (U string #f) integer string boolean -> (U symbol void) (listof check-results)
(define (parse-type:symbol+integer/case case-sensitive?)
  (make-parse-type (string-append "integer or " (if case-sensitive? "symbol" "symbol (case-insensitive)"))
    (lambda (raw)
      (let-values ([(integer-val _) (parse-type:integer raw)])
        (if (integer? integer-val)
            integer-val            
            (let-values ([(symbol-val _) ((if case-sensitive? parse-type:symbol parse-type:symbol-ci) raw)])
              (if (symbol? symbol-val)
                  symbol-val
                  (parse-fail))))))))

; string csv-column -> (U symbol void) (listof check-results)
(define parse-type:symbol+integer
  (parse-type:symbol+integer/case #t))

; string csv-column -> (U symbol void) (listof check-results)
(define parse-type:symbol-ci+integer
  (parse-type:symbol+integer/case #f))



; Parse-type procedure creators ------------------


; (listof symbol) -> ((U String #f) csv-column -> (U symbol void) (listof check-results))
(define (make-parse-type/symbols-ci symbols)
  (make-parse-type (format "one of (~s)" (string-join (map symbol->string symbols) ", "))    
    (lambda (raw)
      (let* ([trimmed-raw (string-trim-both raw)]
             [sym-match   (for/or ([sym (in-list symbols)])
                            (and (string-ci=? (symbol->string sym) trimmed-raw) sym))])
        (if (symbol? sym-match)
            sym-match
            (parse-fail))))))

; (listof symbol) -> ((U String #f) csv-column -> (U symbol void #f) (listof check-results))
(define (make-parse-type/symbols-ci+false symbols)
  (let ([parse-type:wrapped (make-parse-type/symbols-ci symbols)])
    (make-parse-type (format "one of (~s), or <blank>" (string-join (map symbol->string symbols) ", "))
      (lambda (raw)
        (if (or (not raw) (equal? raw ""))
            #f
            ((parse-type-criteria parse-type:wrapped) raw))))))


; Pairs of (symbol . (U symbol boolean)) ---------

; (listof (alist string . any)) boolean -> ((U String #f) csv-column -> any (listof check-results))
(define (make-parse-type/pairs/case in->out case-sensitive?)
  (make-parse-type (format "one of (~a)" (string-join (map (lambda (arg) (format "~s" (car arg))) in->out) " "))
    (lambda (raw)
      (let* ([trimmed-raw    (string-trim-both raw)]
             [raw/case       (if case-sensitive? trimmed-raw (string-downcase trimmed-raw))]
             [car/case       (if case-sensitive? car (lambda (lis) (string-downcase (car lis))))]
             [matched-result (for/or ([a-pair (in-list in->out)])
                               (and (equal? raw/case (car/case a-pair)) (cdr a-pair)))])
        (if matched-result
            matched-result
            (parse-fail))))))


; (listof (alist string . any)) -> ((U String #f) csv-column -> any (listof check-results))
(define (make-parse-type/pairs in->out)
  (make-parse-type/pairs/case in->out #t))


; (listof (alist string . any)) -> ((U String #f) csv-column -> any (listof check-results))
(define (make-parse-type/pairs-ci in->out)
  (make-parse-type/pairs/case in->out #f))


; Enumerations -----------------------------------

; enum -> ((U String #f) csv-column -> (U symbol #f void) (listof check-results))
(define (make-parse-type/enum-value enum)
  (make-parse-type (format "one of (~s)" 
                           (string-join (map (lambda (sym)
                                               (cond [(not sym)      "<blank>"]
                                                     [(boolean? sym) "yes"]
                                                     [else           (symbol->string sym)]))
                                             (enum-values enum)) 
                                        "\"/\""))
    (lambda (raw)
      (let* ([trimmed-raw (string-trim-both raw)]
             [enum-match  (for/fold ([result (void)])
                            ([enum-val (in-list (enum-values enum))])
                            (if (and (void? result)
                                     (or (and (not enum-val) (equal? "" trimmed-raw))
                                         (and enum-val (string-ci=? (symbol->string enum-val) trimmed-raw))))
                                enum-val
                                result))])
        (if (enum-value? enum enum-match)
            enum-match
            (parse-fail))))))


; enum -> ((U String #f) csv-column -> (U string #f void) (listof check-results))
(define (make-parse-type/enum-pretty-value enum)
  (make-parse-type (format "one of (~s)" (string-join (enum-pretty-values enum) "\"/\""))
    (lambda (raw)
      (let ([enum-match (for/fold ([result (void)])
                          ([enum-str (in-list (enum-pretty-values enum))]
                           [enum-val (in-list (enum-values        enum))])
                          (if (and (void? result) (string-ci=? enum-str (string-trim-both raw)))
                              enum-val
                              result))])
        (if (enum-value? enum enum-match) 
            enum-match
            (parse-fail))))))


; Provides ---------------------------------------

(provide opt-check)

(provide/contract  
 ; string 
 [parse-type:string                 (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c string? void?) list?))]
 [parse-type:string+false           (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c string? false/c void?) list?))]
 ; symbol
 [parse-type:symbol                 (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c symbol? false/c void?) list?))]
 [parse-type:symbol+false           (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c symbol? false/c void?) list?))]
 ; symbol
 [parse-type:symbol-ci              (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c symbol? false/c void?) list?))]
 [parse-type:symbol-ci+false        (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c symbol? false/c void?) list?))]
 ; numbers
 [parse-type:number                 (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c number? void?) list?))]
 [parse-type:number+false           (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c number? false/c void?) list?))]
 ; integer
 [parse-type:integer                (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c integer? void?) list?))]
 [parse-type:integer+false          (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c integer? false/c void?) list?))]
 ; real
 [parse-type:real                   (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c real? void?) list?))]
 [parse-type:real+false             (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c real? false/c void?) list?))]
 ; compounds
 [parse-type:symbol+integer         (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c symbol? integer? void?) list?))]
 [parse-type:symbol-ci+integer      (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c symbol? integer? void?) list?))]
 ; boolean
 [parse-type:boolean                (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c boolean? void?) list?))]
 [parse-type:boolean+unspecified    (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c boolean? void?) list?))]
 
 ; boolean
 [parse-type:md5                    (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c string? void?) list?))]
 [parse-type:md5+false              (-> (or/c string? false/c) integer? csv-column? 
                                        (values (or/c string? false/c void?) list?))]
 ; lists of symbols
 [make-parse-type/symbols-ci        (-> (listof symbol?) 
                                        (-> (or/c string? false/c) integer? csv-column?
                                            (values (or/c boolean? symbol? void?) list?)))]
 [make-parse-type/symbols-ci+false  (-> (listof symbol?) 
                                        (-> (or/c string? false/c) integer? csv-column?
                                            (values (or/c boolean? symbol? void?) list?)))]
 ; arbitrary pairs
 [make-parse-type/pairs             (-> list?
                                        (-> (or/c string? false/c) integer? csv-column?
                                            (values (or/c boolean? symbol? void?) list?)))]
 [make-parse-type/pairs-ci          (-> list?
                                        (-> (or/c string? false/c) integer? csv-column?
                                            (values (or/c boolean? symbol? void?) list?)))]
 ; enumerations
 [make-parse-type/enum-value        (-> enum? 
                                        (-> (or/c string? false/c) integer? csv-column?
                                            (values (or/c boolean? symbol? void?) list?)))]
 [make-parse-type/enum-pretty-value (-> enum? 
                                        (-> (or/c string? false/c) integer? csv-column?
                                            (values (or/c boolean? symbol? void?) list?)))])