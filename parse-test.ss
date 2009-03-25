#lang scheme/base

(require (except-in srfi/1 any delete!)
         (planet schematics/schemeunit:2/test)
         (planet schematics/schemeunit:2/util)
         "core.ss"
         "parse.ss")

(require/expose "parse.ss" (make-all-column-types))

; Tests ------------------------------------------

; csv-column ...
(define-values (a b c d e)
  (values (make-csv-column "a" parse-type:boolean)
          (make-csv-column "b" parse-type:integer)
          (make-csv-column "c" parse-type:symbol)
          (make-csv-column "d" parse-type:string)
          (make-csv-column "e" parse-type:real)))

(define parse-tests
  (test-suite "parse.ss"
    
    (test-case "make-all-column-types"
      (check-equal? (make-all-column-types (list 1 2 3) (list a b c) #f)
                    (list a b c))
      
      (check-equal? (make-all-column-types (list 1 2 3) (list a b c) parse-type:number)
                    (list a b c))
      
      (check-equal? (make-all-column-types (list 1 2 3 4 5) (list a b c) parse-type:number)
                    (list a b c
                          (make-csv-column "" parse-type:number)
                          (make-csv-column "" parse-type:number))))))

; Provide statements -----------------------------

(provide parse-tests)
