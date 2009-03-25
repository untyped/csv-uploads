#lang scheme/base

(require (except-in srfi/1 any delete!)
         (only-in srfi/26 cut)
         (planet schematics/schemeunit:2/test)
         (planet schematics/schemeunit:2/util)
         (planet untyped/unlib:3/exn)
         "core.ss"
         "read.ss")

; Tests ------------------------------------------

(define read-tests
  (test-suite "read.ss"
    
    (test-exn "read-csv: bad escape sequence"
      exn:fail?
      (cut read-csv (string->bytes/utf-8 "a,\"\\\"b\",c") 3 #t))
    
    (test-equal? "read-csv: no problems (including good escape sequence)"
      (read-csv (string->bytes/utf-8 #<<ENDCSV
a,b
a,b,c,d
1,"2,3",4

 a , "b,""c,d" , e 
ENDCSV
                                     )
                3
                #t)
      (list (make-csv-line/raw 1 "a,b" null (list "a" "b" ""))
            (make-csv-line/raw 2 "a,b,c,d" null (list "a" "b" "c"))
            (make-csv-line/raw 3 "1,\"2,3\",4" null (list "1" "2,3" "4"))
            (make-csv-line/raw 4 "" null (list "" "" ""))
            (make-csv-line/raw 5 " a , \"b,\"\"c,d\" , e " null (list "a" "b,\"c,d" "e"))))
    
    (test-equal? "read-csv: trim-lines? is #t"
      (read-csv (string->bytes/utf-8 #<<ENDCSV
a,b
a,b,c
a,b,c,d
ENDCSV
                                     )
                3
                #t)
      (list (make-csv-line/raw 1 "a,b" null (list "a" "b" ""))
            (make-csv-line/raw 2 "a,b,c" null (list "a" "b" "c"))
            (make-csv-line/raw 3 "a,b,c,d" null (list "a" "b" "c"))))
    
    (test-equal? "read-csv: trim-lines? is #f"
      (read-csv (string->bytes/utf-8 #<<ENDCSV
a,b
a,b,c
a,b,c,d
ENDCSV
                                     )
                3
                #f)
      (list (make-csv-line/raw 1 "a,b" null (list "a" "b" ""))
            (make-csv-line/raw 2 "a,b,c" null (list "a" "b" "c"))
            (make-csv-line/raw 3 "a,b,c,d" null (list "a" "b" "c" "d"))))))

; Provide statements -----------------------------

(provide read-tests)
