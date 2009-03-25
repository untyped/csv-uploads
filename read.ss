#lang scheme/base

(require scheme/list
         scheme/contract
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/number)
         (planet untyped/unlib:3/exn)
         (prefix-in csv: (file "csv.plt-2.0-hacked/csv.ss"))
         "core.ss")

; Procedures -------------------------------------

; bytes integer #:trim-lines? boolean -> (listof csv-line/raw) | exn:fail
(define (read-csv byt num-fields trim-lines?)
  ; There are a couple of handle-and-reraise blocks below that cover
  ; common problems:
  ;   - bytes->string/utf-8 failed (typically due to a binary or non-UTF-8 file);
  ;   - CSV line read failed (typically due to mismatched quotes).
  (with-handlers ([exn? (lambda (exn)
                          (error "Could not read CSV file. Reason: ~s" (exn-message exn)))])
    
    ; string
    (define str
      ; bytes->string/utf-8 throws an exception if the file isn't UTF-8:
      (with-handlers ([exn:fail:contract? 
                       (lambda (exn)
                         (raise-exn exn:fail:contract
                           (string-append "The file was not plain (ASCII or UTF-8) text. "
                                          "If you are uploading data from Excel, make sure you selected a CSV "
                                          "version and not an XLS file.")))])
        (bytes->string/utf-8 byt)))
    
    ; input-port
    (define original-input 
      (open-input-string str))
    
    ; input-port -> (-> (listof string))
    (define make-reader
      (csv:make-csv-reader-maker
       '((separator-chars            . (#\,))
         (strip-leading-whitespace?  . #t)
         (strip-trailing-whitespace? . #t))))
    
    ; (-> (listof string))
    (define read-fields
      (make-reader (open-input-string str)))
    
    ; (listof string) -> (listof string)
    (define (pad-fields fields)
      (if (< (length fields) num-fields)
          (list-pad-right fields num-fields "")
          (if trim-lines?
              (take fields num-fields)
              fields)))
    
    ; Main procedure body
    (let loop ([line-number 1] [accum null])
      (let ([original (read-line original-input 'any)]
            [fields   (with-handlers ([exn? (lambda (exn)
                                              (raise-exn exn:fail 
                                                         (format "Error on line ~a: ~s"
                                                                 line-number
                                                                 (exn-message exn))))])
                        (read-fields))])
        (if (or (eof-object? original)
                (null? fields))
            (reverse accum)
            (loop (add1 line-number)
                  (cons (make-csv-line/raw line-number 
                                           original
                                           null
                                           (pad-fields fields))
                        accum)))))))

; Provide statements -----------------------------

(provide/contract
 [read-csv (-> bytes? natural? boolean? (listof csv-line/raw?))])
