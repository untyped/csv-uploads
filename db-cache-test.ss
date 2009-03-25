#lang scheme/base

(require scheme/async-channel
         (planet schematics/schemeunit:2/test)
         (planet schematics/schemeunit:2/util)
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/hash)
         "db-cache.ss")

; a cache that contains 2 values for a and b, starting at 0 and 1,
; incrementing each time the cache is refreshed.
; db-cache
(define a-cache
  (make-db-cache
   (let ([counter 0])
     (lambda ()
       (begin0 
         (make-hasheq/alist
          `((a . ,counter)
            (b . ,(+ counter 1))))
         (set! counter (+ counter 2)))))))

(define db-cache-tests   
  (test-suite "db-cache.ss"            
    
    ; test retrieval
    (test-case "db-cache retrieval by prop:procedure working"        
      (let ([new-cache (make-db-cache (lambda () (make-hasheq/alist `((a . 1) (b . 2)))))])
        ; test all values
        (check-equal? (new-cache 'a) 1 (format "Incorrect value for 'a: expected 1; found ~a" (new-cache 'a)))
        (check-equal? (new-cache 'b) 2 (format "Incorrect value for 'b: expected 2; found ~a" (new-cache 'b)))
        (check-false  (new-cache 'c) (format "Incorrect value for 'c: expected #f; found ~a" (new-cache 'c)))))
    
    ; test insertion
    (test-case "db-cache insertion working"
      (let ([new-cache (make-db-cache (lambda () (make-hasheq/alist `((a . 1) (b . 2)))))])
        ; test all values
        (check-equal? (new-cache 'a) 1 (format "Incorrect value for 'a: expected 1; found ~a" (new-cache 'a)))
        (check-equal? (new-cache 'b) 2 (format "Incorrect value for 'b: expected 2; found ~a" (new-cache 'b)))
        (check-false  (new-cache 'c) (format "Incorrect value for 'c: expected #f; found ~a" (new-cache 'c)))
        
        ; insert
        (db-cache-insert! new-cache 'c 3)
        
        ; retest all values
        (check-equal? (new-cache 'a) 1 (format "Incorrect value for 'a: expected 1; found ~a" (new-cache 'a)))
        (check-equal? (new-cache 'b) 2 (format "Incorrect value for 'b: expected 2; found ~a" (new-cache 'b)))
        (check-equal? (new-cache 'c) 3 (format "Incorrect value for 'c: expected 3; found ~a" (new-cache 'c)))))
    
    ; cache without clear yields consistent results
    (test-case "Using the same cache within the same thread-cell yields same results"
      (let ([cache-val (a-cache 'a)])
        (check-equal? cache-val 0 (format "Unexpected uncaching result [1]. Expected 0 found ~s" cache-val)))
      (let ([cache-val (a-cache 'a)])
        (check-equal? cache-val 0 (format "Unexpected uncaching result [2]. Expected 0 found ~s" cache-val)))
      (let ([cache-val (a-cache 'a)])
        (check-equal? cache-val 0 (format "Unexpected uncaching result [3]. Expected 0 found ~s" cache-val)))
      (let ([cache-val (a-cache 'a)])
        (check-equal? cache-val 0 (format "Unexpected uncaching result [4]. Expected 0 found ~s" cache-val))))
    
    
    ; manually clearing cache yields different but expected results
    (test-case "Manually clearing the cache between accesses causes refreshed cache values"
      (let ([cache-val (a-cache 'a)])        
        (check-equal? cache-val 0 (format "Unexpected uncaching result [5]. Expected 0 found ~s" cache-val)))
      (clear-db-cache! a-cache)
      
      (let ([cache-val (a-cache 'a)])
        (check-equal? cache-val 2 (format "Unexpected uncaching result [6]. Expected 2 found ~s" cache-val)))
      (clear-db-cache! a-cache)
      
      (let ([cache-val (a-cache 'a)])
        (check-equal? cache-val 4 (format "Unexpected uncaching result [7]. Expected 4 found ~s" cache-val)))
      (clear-db-cache! a-cache)
      
      (let ([cache-val (a-cache 'a)])
        (check-equal? cache-val 6 (format "Unexpected uncaching result [8]. Expected 6 found ~s" cache-val))
        (clear-db-cache! a-cache)))
    
    
    ; cache without clear yields consistent results
    (test-case "Using call-with-clear-db-cache causes cache refreshing"
      (let ([cache-val (call-with-clear-db-cache (lambda () (a-cache 'a)))])
        (check-equal? cache-val 8 (format "Unexpected uncaching result [9]. Expected 8 found ~s" cache-val)))
      (let ([cache-val (call-with-clear-db-cache (lambda () (a-cache 'a)))])
        (check-equal? cache-val 10 (format "Unexpected uncaching result [10]. Expected 10 found ~s" cache-val)))
      (let ([cache-val (call-with-clear-db-cache (lambda () (a-cache 'a)))])
        (check-equal? cache-val 12 (format "Unexpected uncaching result [11]. Expected 12 found ~s" cache-val)))
      (let ([cache-val (call-with-clear-db-cache (lambda () (a-cache 'a)))])
        (check-equal? cache-val 14 (format "Unexpected uncaching result [12]. Expected 14 found ~s" cache-val))))))


; Provides ---------------------------------------

(provide db-cache-tests)