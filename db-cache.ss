#lang scheme/base

(require scheme/async-channel
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/hash))

; (struct (thread-cell (hashof key val))
;         (-> (hashof key val))
;         (key -> val))
(define-struct db-cache (cell init-proc default-proc)
  #:transparent
  #:property prop:custom-write
  (lambda (struct out write?)
    (let ([show (if write? write display)])
      (display "#(db-cache " out)
      (show (thread-cell-ref (db-cache-cell struct)) out)
      (display ")" out)))
  #:property prop:procedure
  (lambda (db-cache key)
    (let ([cell (db-cache-cell db-cache)])
      (unless (thread-cell-ref cell)
        (thread-cell-set! cell ((db-cache-init-proc db-cache))))
      (hash-ref (thread-cell-ref cell)
                key
                (lambda ()
                  ((db-cache-default-proc db-cache) key))))))

;  (-> (hashof key val))
;  [(key -> val)]
; ->
;  db-cache
(define (create-db-cache init-proc [default-proc (lambda _ #f)])
  (make-db-cache (make-thread-cell #f)
                 init-proc
                 default-proc))

; db-cache any any -> void
(define (db-cache-insert! db-cache key val)
  (let ([cell (db-cache-cell db-cache)])
    (unless (thread-cell-ref cell)
      (thread-cell-set! cell ((db-cache-init-proc db-cache))))
    (hash-set! (thread-cell-ref cell) key val)))

; db-cache -> void
(define (clear-db-cache! db-cache)
  (thread-cell-set! (db-cache-cell db-cache) #f))

; db-cache -> sequence
(define (in-db-cache-keys db-cache)  
  (let ([cell (db-cache-cell db-cache)])
    (if (thread-cell-ref cell)
        (in-hash-keys (thread-cell-ref cell))
        (in-list null))))

; (-> any) -> any
(define (call-with-clear-db-cache thunk)
  (let ([channel (make-async-channel)])
    (thread (lambda ()
              (async-channel-put channel (thunk))))
    (async-channel-get channel)))

; Provides ---------------------------------------

(provide (except-out (struct-out db-cache) make-db-cache)
         (rename-out [create-db-cache make-db-cache])
         db-cache-insert!
         clear-db-cache!
         in-db-cache-keys
         call-with-clear-db-cache)