#lang racket/base
; png-image/png-image-test.rkt
; test file for the png-image library
(require rackunit racket/file "../png-image.rkt")

(define MAGIC-NUMBER (bytes 137 80 78 71 13 10 26 10))
(define MAGIC-LEN (bytes-length MAGIC-NUMBER))
(define racket-logo "Racket.png")

(check-true (png? racket-logo))

; the bytes won't be exact, but at least the magic number and IHDR
; should be intact
(let* ([png-hash (png->hash racket-logo)]
       [png-bytes (hash->png png-hash)]
       [png-file (file->bytes racket-logo)])
  (define IHDR-hash (hash-ref png-hash 'IHDR))
  (define IHDR-len (+ 12 (hash-ref IHDR-hash 'length)))
  (check-equal? (subbytes png-bytes 0 (+ MAGIC-LEN IHDR-len))
                (subbytes png-file 0 (+ MAGIC-LEN IHDR-len))))
