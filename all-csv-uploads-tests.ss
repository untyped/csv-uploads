#lang scheme/base

(require (planet schematics/schemeunit:2/test)
         (planet schematics/schemeunit:2/util)
         "db-cache-test.ss"
         "read-test.ss"
         "parse-test.ss"
         "generic-parse-types-test.ss")

; test-suite
(define all-csv-uploads-tests
  (test-suite "CSV library tests"
    db-cache-tests    
    read-tests
    parse-tests
    parse-util-tests))

; Provide statements -----------------------------

(provide all-csv-uploads-tests)
