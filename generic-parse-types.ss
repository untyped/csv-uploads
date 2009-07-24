#lang scheme/base

(require (for-syntax scheme/base)         
         scheme/contract
         scheme/string
         (only-in srfi/13 string-trim-both)
         (planet untyped/snooze:3)
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
 [parse-type:string                 (parse-type/c (or/c string? void?))]
 [parse-type:string+false           (parse-type/c (or/c string? false/c void?))]
 ; symbol
 [parse-type:symbol                 (parse-type/c (or/c symbol? false/c void?))]
 [parse-type:symbol+false           (parse-type/c (or/c symbol? false/c void?))]
 ; symbol
 [parse-type:symbol-ci              (parse-type/c (or/c symbol? false/c void?))]
 [parse-type:symbol-ci+false        (parse-type/c (or/c symbol? false/c void?))]
 ; numbers
 [parse-type:number                 (parse-type/c (or/c number? void?))]
 [parse-type:number+false           (parse-type/c (or/c number? false/c void?))]
 ; integer
 [parse-type:integer                (parse-type/c (or/c integer? void?))]
 [parse-type:integer+false          (parse-type/c (or/c integer? false/c void?))]
 ; real
 [parse-type:real                   (parse-type/c (or/c real? void?))]
 [parse-type:real+false             (parse-type/c (or/c real? false/c void?))]
 ; compounds
 [parse-type:symbol+integer         (parse-type/c (or/c symbol? integer? void?))]
 [parse-type:symbol-ci+integer      (parse-type/c (or/c symbol? integer? void?))]
 ; boolean
 [parse-type:boolean                (parse-type/c (or/c boolean? void?))]
 [parse-type:boolean+unspecified    (parse-type/c (or/c boolean? void?))]
 
 ; boolean
 [parse-type:md5                    (parse-type/c (or/c string? void?))]
 [parse-type:md5+false              (parse-type/c (or/c string? false/c void?))]
 ; lists of symbols
 [make-parse-type/symbols-ci        (-> (listof symbol?) 
                                        (parse-type/c (or/c boolean? symbol? void?)))]
 [make-parse-type/symbols-ci+false  (-> (listof symbol?) 
                                        (parse-type/c (or/c boolean? symbol? void?)))]
 ; arbitrary pairs
 [make-parse-type/pairs             (-> list?
                                        (parse-type/c (or/c boolean? symbol? void?)))]
 [make-parse-type/pairs-ci          (-> list?
                                        (parse-type/c (or/c boolean? symbol? void?)))]
 ; enumerations
 [make-parse-type/enum-value        (-> enum? 
                                        (parse-type/c (or/c boolean? symbol? void?)))]
 [make-parse-type/enum-pretty-value (-> enum? 
                                        (parse-type/c (or/c boolean? symbol? void?)))])