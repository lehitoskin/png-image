#lang racket/base
; png-image.rkt
(require file/gunzip
         file/gzip
         file/sha1
         racket/contract
         racket/file
         racket/format
         racket/port
         "base.rkt"
         "txt.rkt")
(provide png? png->hash hash->png)

; info about PNG chunk definitions
; http://dev.exiv2.org/projects/exiv2/wiki/The_Metadata_in_PNG_files
; LENGTH - 4 bytes
; CHUNK TYPE - 4 bytes
; CHUNK DATA - LENGTH bytes
; CRC32 - 4 bytes

; determines if a given file is a PNG
(define/contract (png? img)
  ((or/c path-string? bytes?) . -> . boolean?)
  (define in-port (if (bytes? img)
                      (open-input-bytes img)
                      (open-input-file img)))
  (define bstr (peek-bytes MAGIC-LEN 0 in-port))
  (close-input-port in-port)
  (bytes=? bstr MAGIC-NUMBER))

; takes a byte string and returns a hash with the chunk information
(define (chunk-info bstr)
  (define in-port (open-input-bytes bstr))
  (define data-len (bytes->number (peek-bytes 4 0 in-port)))
  (define type (peek-bytes 4 4 in-port))
  (define data (peek-bytes data-len 8 in-port))
  (define crc32 (peek-bytes 4 (+ 8 data-len) in-port))
  (close-input-port in-port)
  (hasheq 'type type
          'data data
          'length data-len
          'crc32 crc32))

; given a path-string (or bytes), return a hash with each chunk
; separated by its identifier. any chunks that may appear
; multiple times are represented as a list of hashes
(define/contract (png->hash img [hsh (hash)])
  (png? . -> . hash?)
  (define bstr (if (bytes? img) img (file->bytes img)))
  (cond [(bytes=? bstr #"") hsh]
        [(bytes=? (subbytes bstr 0 MAGIC-LEN) MAGIC-NUMBER)
         ; at the beginning of the PNG, skip some bytes
         (png->hash (subbytes bstr MAGIC-LEN) hsh)]
        [else
         ; we are not at the beginning of the PNG
         (define info (chunk-info bstr))
         (define type-sym (string->symbol (bytes->string/utf-8 (hash-ref info 'type))))
         ; length of the data, plus the other parts of the chunk
         (define info-len (+ 12 (hash-ref info 'length)))
         (define accum
           (if (member type-sym multiples)
               (if (hash-has-key? hsh type-sym)
                   (append (hash-ref hsh type-sym)
                           (case type-sym
                             [(tEXt)
                              (list (make-text-hash (subbytes bstr 0 info-len)))]
                             [(zTXt)
                              (list (make-ztxt-hash (subbytes bstr 0 info-len)))]
                             [(iTXt)
                              (list (make-itxt-hash (subbytes bstr 0 info-len)))]
                             [else (list info)]))
                   (if (member type-sym text-chunks)
                       (list (make-itxt-hash (subbytes bstr 0 info-len)))
                       (list info)))
               info))
         (png->hash (subbytes bstr info-len) (hash-set hsh type-sym accum))]))

; return a hash to a byte string
(define/contract (hash->png hsh)
  (hash? . -> . bytes?)
  (with-output-to-bytes
      (λ ()
        (display MAGIC-NUMBER)
        ; to make life easier, just go down the
        ; list of chunks in order
        (for ([key (in-list chunk-order)])
          (when (hash-has-key? hsh key)
            (define val (hash-ref hsh key))
            (if (list? val)
                (map (λ (h)
                       (case key
                         [(tEXt)
                          (define inner (hash-ref h 'data))
                          (display
                           (make-text-chunk
                            (bytes->string/latin-1 (hash-ref inner 'keyword))
                            (bytes->string/latin-1 (hash-ref inner 'text))))]
                         [(zTXt)
                          (define inner (hash-ref h 'data))
                          (display
                           (make-ztxt-chunk
                            (bytes->string/latin-1 (hash-ref inner 'keyword))
                            (bytes->string/latin-1 (hash-ref inner 'text))))]
                         [(iTXt)
                          (define inner (hash-ref h 'data))
                          (display
                           (make-itxt-chunk
                            (bytes->string/utf-8 (hash-ref inner 'keyword))
                            (bytes->string/utf-8 (hash-ref inner 'text))
                            (bytes->string/utf-8 (hash-ref inner 'language-tag))
                            (bytes->string/utf-8 (hash-ref inner 'translated-keyword))))]
                         [else
                          (printf "~a~a~a~a"
                                  (number->bytes (hash-ref h 'length))
                                  (hash-ref h 'type)
                                  (hash-ref h 'data)
                                  (hash-ref h 'crc32))]))
                     val)
                (printf "~a~a~a~a"
                        (number->bytes (hash-ref val 'length))
                        (hash-ref val 'type)
                        (hash-ref val 'data)
                        (hash-ref val 'crc32))))))))
