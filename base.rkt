#lang racket/base
; base.rkt
(require file/sha1 racket/format)
(provide (all-defined-out))

(define MAGIC-NUMBER (bytes 137 80 78 71 13 10 26 10))
(define MAGIC-LEN (bytes-length MAGIC-NUMBER))

(define IHDR #"IHDR") ; width, height, and bit-depth
(define PLTE #"PLTE") ; the list of colors
(define IDAT #"IDAT") ; the actual image data (may be split in multiple IDAT chunks)
(define IEND #"IEND") ; marks the end of the image - chunk's data field is empty

; list of chunks that may be in the PNG more than once
(define multiples '(IDAT
                    sPLT
                    iTXt
                    tEXt
                    zTXt))

; list of text chunks
(define text-chunks '(iTXt tEXt zTXt))

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
                      ; may place text chunks after IDAT in special circumstances
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
