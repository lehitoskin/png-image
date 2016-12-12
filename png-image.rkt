#lang racket/base
; png-image.rkt
(require file/sha1 racket/contract racket/file racket/format racket/port)
(provide png?
         png->hash
         hash->png)

(define MAGIC-NUMBER (bytes 137 80 78 71 13 10 26 10))
(define MAGIC-LEN (bytes-length MAGIC-NUMBER))

; info about PNG chunk definitions
; http://dev.exiv2.org/projects/exiv2/wiki/The_Metadata_in_PNG_files
; LENGTH - 4 bytes
; CHUNK TYPE - 4 bytes
; CHUNK DATA - LENGTH bytes
; CRC - 4 bytes

(define IHDR #"IHDR") ; width, height, and bit-depth
(define PLTE #"PLTE") ; the list of colors
(define IDAT #"IDAT") ; the actual image data (may be split in multiple IDAT chunks)
(define IEND #"IEND") ; marks the end of the image - chunk's data field is empty

; list of chunks that may be in the PNG more than once
(define multiples '(IDAT
                    sPLT ; must be placed before IDAT
                    iTXt
                    tEXt
                    zTXt))

; order of chunks that appear in a PNG
(define chunk-order '(; required, must be first
                      IHDR
                      
                      ; ancillary chunks, before PLTE
                      cHRM
                      gAMA
                      iCCP
                      sBIT
                      sRGB
                      
                      ; required, before IDAT
                      PLTE

                      ; ancillary chunks, after PLTE and before IDAT
                      bGKD
                      hIST
                      tRNS
                      ; ancillary chunks, before IDAT
                      pHYS
                      sPLT
                      ; ancillary chunks, no special order
                      tIME
                      iTXt
                      tEXt
                      zTXt
                      
                      ; required, may appear more than once
                      IDAT
                      ; required, must be last
                      IEND))

; takes a byte string and turns it into a decimal number
(define (bytes->number bstr)
  (string->number (bytes->hex-string bstr) 16))

; takes a decimal number and turns it into a byte string
(define (number->bytes num)
  (define str (~r num #:base 16 #:min-width 8 #:pad-string "0"))
  (hex-string->bytes str))

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
  (define crc (peek-bytes 4 (+ 8 data-len) in-port))
  (close-input-port in-port)
  (hasheq 'type type
          'data data
          'length data-len
          'crc crc))

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
         (png->hash
          (subbytes bstr info-len)
          (hash-set hsh
                    type-sym
                    (if (member type-sym multiples)
                        (if (hash-has-key? hsh type-sym)
                            (append (hash-ref hsh type-sym) (list info))
                            (list info))
                        info)))]))

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
                       (printf "~a~a~a~a"
                               (number->bytes (hash-ref h 'length))
                               (hash-ref h 'type)
                               (hash-ref h 'data)
                               (hash-ref h 'crc)))
                     val)
                (printf "~a~a~a~a"
                        (number->bytes (hash-ref val 'length))
                        (hash-ref val 'type)
                        (hash-ref val 'data)
                        (hash-ref val 'crc))))))))
