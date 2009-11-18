#lang scheme/base

(require (planet schematics/schemeunit:2/test)
         (planet schematics/schemeunit:2/util)
         (planet untyped/snooze:3)
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/enum)
         "generic-parse-types.ss"
         "core.ss")

; Enumerations -----------------------------------

; enum ...
(define-enum alphabet
  ([a 'a "ay"]
   [b 'b "bee"]
   [c 'c "see"]))

(define-enum alphabet/false
  ([x #f "ex"]
   [y 'y "why"]
   [z 'z "zed"]))

; (listof (string . symbol))
(define pairs `(("a" . one) ("b" . two) ("c" . three)))

; (listof symbol) ...
(define symbols '(a b c d))
(define symbols-ci '(aB bA Ac d))

; Test suites ------------------------------------

(define parse-util-tests       
  (test-suite "generic-parse-types.ss" 
    
    ; Strings ------------------------------------
    
    ; TEST: string (not false)
    (test-case "parse-type:string"
      (check-valid-values parse-type:string (list "i am a string" "1") (list "i am a string" "1"))
      (check-invalid-values parse-type:string (list #f ""))
      
      ; valid values, but case sensitive
      (let*-values ([(str-in str-out) (values "i am a string" "I Am A String")]
                    [(data problems) (parse-type:string str-in 1 (make-csv-column "test column" parse-type:string))])
        (check-false (equal? data str-out)      (format "Parse mismatch: ~s -> ~s" str-in str-out))
        (check-false (check-problems? problems) (format "Parse problems: ~s generates problems ~a" str-in problems))))
    
    
    ; TEST string or false
    (test-case "parse-type:string+false"
      (check-valid-values parse-type:string+false (list "i am a string" "1" #f "") (list "i am a string" "1" #f #f)))        
    
    
    ; Symbols ------------------------------------
    
    ; TEST: symbols
    (test-case "parse-type:symbol"      
      (check-valid-values parse-type:symbol (list "iamasymbol" "1") (list 'iamasymbol '|1|))
      (check-invalid-values parse-type:symbol (list "" #f)))    
    
    ; TEST: symbol or false
    (test-case "parse-type:symbol+false"
      (check-valid-values parse-type:symbol+false (list "iamasymbol" "1" #f "") (list 'iamasymbol '|1| #f #f)))
    
    ; TEST: symbol-ci
    (test-case "parse-type:symbol-ci"
      (check-valid-values parse-type:symbol-ci
                          (list "iamasymbol" "IamAsymBol" "1")
                          (list 'iamasymbol 'iamasymbol '|1|))
      (check-invalid-values parse-type:symbol-ci (list "" #f))) 
    
    ; TEST: symbol-ci or false
    (test-case "parse-type:symbol-ci+false"
      (check-valid-values parse-type:symbol-ci+false
                          (list "iamasymbol" "IamAsymBol" "1" #f "") 
                          (list 'iamasymbol 'iamasymbol '|1| #f #f)))
    
    
    ; Numbers ------------------------------------
    
    ; TEST: number (not including false)
    (test-case "parse-type:number"
      (check-valid-values parse-type:number (list "123" "1.23") (list 123 1.23))
      (check-invalid-values parse-type:number (list #f "" "hello" "1n" "1.n3")))
    
    ; TEST: number or false
    (test-case "parse-type:number+false" 
      (check-valid-values parse-type:number+false (list "123" "1.23" #f "") (list 123 1.23 #f #f))
      (check-invalid-values parse-type:number+false (list "hello" "1n" "1.n3"))) 
    
    ; TEST: integer (not including false)
    (test-case "parse-type:integer"
      (check-valid-values parse-type:integer (list "123") (list 123))
      (check-invalid-values parse-type:integer (list "1.23" #f "" "hello" "1n" "1.n3")))
    
    ; TEST: integer or false
    (test-case "parse-type:integer+false" 
      (check-valid-values parse-type:integer+false (list "123" #f "") (list 123 #f #f))
      (check-invalid-values parse-type:integer+false (list "1.23" "hello" "1n" "1.n3"))) 
    
    ; TEST: real (not including false)
    (test-case "parse-type:real"
      (check-valid-values parse-type:real (list "1.23" "123") (list 1.23 123))
      (check-invalid-values parse-type:real (list  #f "" "hello" "1n" "1.n3")))
    
    ; TEST: real or false
    (test-case "parse-type:real+false" 
      (check-valid-values parse-type:real+false (list "1.23" "123" #f "") (list 1.23 123 #f #f))
      (check-invalid-values parse-type:real+false (list "hello" "1n" "1.n3"))) 
    
    
    ; Booleans -----------------------------------
    
    ; TEST: boolean (not including unspecified)
    (test-case "parse-type:boolean"
      (check-valid-values parse-type:boolean
                          (list "true" "TRUE" "True" "yes" "YES" "Yes" "y" "Y"
                                "false" "FALSE" "False" "no" "NO" "No" "n" "N") 
                          (list #t #t #t #t #t #t #t #t 
                                #f #f #f #f #f #f #f #f))
      (check-invalid-values parse-type:boolean (list "#t" "#f" "" "flase" "hello" "1n" "1.n3")))
    
    ; TEST: boolean or unspecified
    (test-case "parse-type:boolean+unspecified"
      (check-valid-values parse-type:boolean+unspecified 
                          (list "true" "TRUE" "True" "yes" "YES" "Yes" "y" "Y"
                                "false" "FALSE" "False" "no" "NO" "No" "n" "N" "")
                          (list #t #t #t #t #t #t #t #t 
                                #f #f #f #f #f #f #f #f #f))
      (check-invalid-values parse-type:boolean+unspecified (list "#t" "#f" "flase" "hello" "1n" "1.n3"))) 
    
    
    ; Compounds ----------------------------------
    
    ; TEST: symbols and integers (not false)
    (test-case "parse-type:symbol+integer"
      (check-valid-values parse-type:symbol+integer (list "a" "A" "1") (list 'a 'A 1))
      (check-invalid-values parse-type:symbol+integer (list "")))
    
    ; TEST: case-insensitive symbols and integers (not false)boolean or unspecified
    (test-case "parse-type:symbol-ci+integer"
      (check-valid-values parse-type:symbol-ci+integer (list "a" "A" "1") (list 'a 'a 1))
      (check-invalid-values parse-type:symbol-ci+integer (list "")))        
    
    
    ; Lists of symbols --------------------------    
    (test-case "make-parse-type/symbols-ci"
      (check-valid-values (make-parse-type/symbols-ci symbols-ci) (map symbol->string symbols-ci) symbols-ci)
      (check-invalid-values (make-parse-type/symbols-ci symbols-ci) (list "" "1" "A")))
    
    (test-case "make-parse-type/symbols-ci+false"
      (check-valid-values (make-parse-type/symbols-ci+false symbols-ci) 
                          (list* "" (map symbol->string symbols-ci)) (list* #f symbols-ci))
      (check-invalid-values (make-parse-type/symbols-ci+false symbols-ci) (list "1" "A")))
    
    
    ; Arbitrary pairs ----------------------------
    
    (test-case "make-parse-type:pairs"
      (check-valid-values (make-parse-type/pairs pairs) (map car pairs) (map cdr pairs))
      (check-invalid-values (make-parse-type/pairs pairs) (list "1" "2" "3" "aa" "")))
    
    ; Enumerations -------------------------------
    
    ; enumeration type, by prettified values
    (test-case "make-parse-type:enum-pretty-values"
      (check-valid-values (make-parse-type/enum-pretty-value alphabet) 
                          (enum-pretty-values alphabet)
                          (enum-values alphabet))
      
      ; enumerations with false
      (check-valid-values (make-parse-type/enum-pretty-value alphabet/false) 
                          (enum-pretty-values alphabet/false)
                          (enum-values alphabet/false))
      
      (check-invalid-values (make-parse-type/enum-pretty-value alphabet) (list "abc")))
    
    
    ; enumeration type, by values
    (test-case "make-parse-type:enum-values"
      (check-valid-values (make-parse-type/enum-value alphabet) 
                          (map symbol->string (enum-values alphabet))
                          (enum-values alphabet))
      
      ; enumerations with false
      (check-valid-values (make-parse-type/enum-value alphabet/false) 
                          (map (lambda (status) (if status (symbol->string status) ""))
                               (enum-values alphabet/false))
                          (enum-values alphabet/false))
      
      (check-invalid-values (make-parse-type/enum-value alphabet) (list "abc")))))

; Validation helpers -----------------------------

; parse-type (listof any) (U (listof any) #f) -> (listof check-results)
(define-check (check-valid-values parse-type inputs outputs)
  (let ([column (make-csv-column "test column" parse-type)])
    (for ([input  (in-list inputs)]
          [output (or (and outputs (in-list outputs)) (in-naturals))])
      (let-values ([(parsed-input problems) (parse-type input 1 column)])
        ; check parsed input against expected output, where specified
        (when outputs (check-true (equal? parsed-input output) (format "Parse mismatch: ~s -> ~s (found ~s)" input output parsed-input)))
        ; check that there are no parse problems
        (check-false (check-problems? problems) (format "Parse problems: ~s generated problems ~a" input problems))))))


; parse-type (listof any) -> (listof check-results)
(define-check (check-invalid-values parse-type inputs)
  (let ([column (make-csv-column "test column" parse-type)])
    (for ([input  (in-list inputs)])
      (let-values ([(parsed-input problems) (parse-type input 1 column)])
        ; check parsed input is void
        (check-true (void? parsed-input) (format "Unexpected non-void misparse: ~s -> ~s" input parsed-input))
        ; check that there are no parse problems
        (check-true (check-problems? problems) (format "Unexpected lack of parse problems: ~s" input))))))

; Provides ---------------------------------------

(provide parse-util-tests)