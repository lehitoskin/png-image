#lang racket/base
; txt.rkt
(require file/gunzip
         file/gzip
         file/sha1
         racket/contract
         racket/port)
(provide bytes-crc32
         bytes-adler32
         itxt-data->hash
         itxt-hash->data
         itxt-set
         make-itxt-chunk
         make-itxt-hash
         make-text-chunk
         make-text-hash
         make-ztxt-chunk
         make-ztxt-hash
         text-data->hash
         text-hash->data
         text-set
         ztxt-data->hash
         ztxt-hash->data
         ztxt-set)

; tEXt:
; LENGTH
; CHUNK TYPE: #"tEXt"
; DATA - Keyword (1 - 79),
;        Null Separator,
;        Text (0 or more bytes)

; zTXt:
; LENGTH of compressed data
; CHUNK TYPE: #"zTXt"
; DATA - Keyword (1 - 79),
;        Null Separator,
;        Compression Method (1 byte) (currently only 0 is valid),
;        Text (0 or more)

; iTXt:
; LENGTH of compressed(?) data
; CHUNK TYPE: #"iTXt"
; DATA - Keyword (1 - 79),
;        Null Separator,
;        Compression Flag (1 byte),
;        Compression Method (1 byte) (ignored if compression flag is 0),
;        Language Tag (0 or more),
;        Null separator,
;        Translated keyword (0 or more),
;        Null separator,
;        Text (0 or more)
; CRC32 - crc32 of chunk type and data
;
; itxt-chunk - complete iTXt chunk
; itxt-data - the inner hash describing the iTXt data
; (hash png-image
;       'iTXt (hash 'type #"iTXt"
;                   ; data is translated from a straight byte string
;                   ; to a hash via (read-itxt-chunk)
;                   'data (hash 'keyword keyword
;                               'compression-flag cflag
;                               'compression-method cmethod
;                               'language-tag ltag
;                               'translated-keyword tkw
;                               'text bstr)
;                   'length length
;                   'crc32 crc32))
; https://www.w3.org/TR/PNG/#11iTXt

; from rosetta code on crc32
(define (bytes-crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
             ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
               ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

#|
https://www.ietf.org/rfc/rfc1950.txt

Adler-32 is composed of two sums accumulated per byte: s1 is
the sum of all bytes, s2 is the sum of all s1 values. Both sums
are done modulo 65521. s1 is initialized to 1, s2 to zero.  The
Adler-32 checksum is stored as s2*65536 + s1 in most-
significant-byte first (network) order.
|#
(define (bytes-adler32 bstr)
  (define ADLER 65521)
  (define-values (s1 s2)
    (for/fold ([s1 1]
               [s2 0])
              ([bits (in-bytes bstr)])
      (define a (modulo (+ s1 bits) ADLER))
      (define b (modulo (+ s2 a) ADLER))
      (values a b)))
  ; (s2 << 16) | s1
  (bitwise-ior (arithmetic-shift s2 16) s1))

; different from number->bytes in base.rkt because with that we need a specific
; width, while with this function we don't particularly care about the width so
; long as it has an even number of digits so it can be processed by
; hex-string->bytes
(define (num->bstr num)
  (define hex (number->string num 16))
  (hex-string->bytes (if (even? (string-length hex)) hex (string-append "0" hex))))

; reads a tEXt bye string and returns the data in a hash
(define/contract (text-data->hash bstr)
  (bytes? . -> . hash?)
  (define bstr-in (open-input-bytes bstr))
  (define kw
    ; loop until we find \0
    ; skip length and type
    (let loop ([offset 8])
      (define byte (peek-bytes 1 offset bstr-in))
      (if (bytes=? byte (bytes 0))
          (peek-bytes (- offset 8) 8 bstr-in)
          (loop (add1 offset)))))
  (define kw-len (bytes-length kw))

  (define data (subbytes bstr
                         (+ 9 kw-len)
                         (- (bytes-length bstr) 4)))
  
  (close-input-port bstr-in)

  (hash 'keyword kw
        'text data))

; reads a zTXt byte string and returns the uncompressed data in a hash
(define/contract (ztxt-data->hash bstr)
  (bytes? . -> . hash?)
  (define bstr-in (open-input-bytes bstr))
  (define kw
    ; loop until we find \0
    ; skip length and type
    (let loop ([offset 8])
      (define byte (peek-bytes 1 offset bstr-in))
      (if (bytes=? byte (bytes 0))
          (peek-bytes (- offset 8) 8 bstr-in)
          (loop (add1 offset)))))
  (define kw-len (bytes-length kw))

  (define compression-method (peek-bytes 1 (+ 9 kw-len) bstr-in))

  (define data (subbytes bstr
                         (+ 10 kw-len)
                         (- (bytes-length bstr) 4)))
  (close-input-port bstr-in)
  (hash 'keyword kw
        'compression-method compression-method
        'text (cond [(bytes=? compression-method (bytes 0))
                     ; uncompress via inflate method
                     (define compressed-in (open-input-bytes (subbytes data 2)))
                     (define compressed-out (open-output-bytes))
                     (inflate compressed-in compressed-out)
                     (define uncompressed (get-output-bytes compressed-out))
                     (close-input-port compressed-in)
                     (close-output-port compressed-out)
                     uncompressed]
                    [else data])))

; reads an iTXt byte string and returns the uncompressed(?) data in a hash
(define/contract (itxt-data->hash bstr)
  (bytes? . -> . hash?)
  (define bstr-in (open-input-bytes bstr))
  (define type (peek-bytes 4 4 bstr-in))
  (define kw
    ; loop until we find \0
    ; skip length and type
    (let loop ([offset 8])
      (define byte (peek-bytes 1 offset bstr-in))
      (if (bytes=? byte (bytes 0))
          (peek-bytes (- offset 8) 8 bstr-in)
          (loop (add1 offset)))))
  (define kw-len (bytes-length kw))
  
  (define compression-flag (peek-bytes 1 (+ 9 kw-len) bstr-in))
  (define compression-method (peek-bytes 1 (+ 10 kw-len) bstr-in))

  ; may be omitted
  (define language-tag
    ; loop until we find \0
    (let loop ([offset (+ 11 kw-len)])
      (define byte (peek-bytes 1 offset bstr-in))
      (cond [(eof-object? byte) #""]
            [(bytes=? byte (bytes 0))
             (define bstrlen (- offset (+ 11 kw-len)))
             (if (= bstrlen 0)
                 #"" ; language tag has been omitted
                 (peek-bytes 1 bstrlen bstr-in))]
            [else
             (loop (add1 offset))])))
  (define ltag-len (bytes-length language-tag))

  ; may be omitted
  (define translated-kw
    ; loop until we fine \0
    (let loop ([offset (+ 12 kw-len ltag-len)])
      (define byte (peek-bytes 1 offset bstr-in))
      (cond [(eof-object? byte) #""]
            [(bytes=? byte (bytes 0))
             (define bstrlen (- offset (+ 12 kw-len ltag-len)))
             (if (= bstrlen 0)
                 #"" ; translated keywork omitted
                 (peek-bytes bstrlen offset bstr-in))]
            [else
             (loop (add1 offset))])))
  (define tkw-len (bytes-length translated-kw))
  
  (define data (subbytes bstr
                         ; skip this many bytes
                         (+ 13 kw-len ltag-len tkw-len)
                         ; and read until 4 bytes before the end (the crc32)
                         (- (bytes-length bstr) 4)))
  ; can't forget to close all the ports
  (close-input-port bstr-in)
  ; return a hash with all the data information
  (hash
   'keyword kw
   'compression-flag compression-flag
   'compression-method compression-method
   'language-tag language-tag
   'translated-keyword translated-kw
   'text
   (cond [(and (bytes=? compression-flag (bytes 1))
               (bytes=? compression-method (bytes 0)))
          ; inflate the compressed data
          (define compressed-in (open-input-bytes (subbytes data 2)))
          (define compressed-out (open-output-bytes))
          (inflate compressed-in compressed-out)
          (define uncompressed (get-output-bytes compressed-out))
          (close-input-port compressed-in)
          (close-output-port compressed-out)
          uncompressed]
         [else data])))

; take the text-data hash and returns a tEXt byte string
(define/contract (text-hash->data hsh)
  (hash? . -> . bytes?)
  (define inner (hash-ref hsh 'data))
  (bytes-append (hash-ref inner 'keyword)
                (bytes 0)
                (hash-ref inner 'text)))

; take the ztxt-data hash and returns a zTXt byte string
; - will be compressed!
(define/contract (ztxt-hash->data hsh)
  (hash? . -> . bytes?)
  (define inner (hash-ref hsh 'data))
  ; deflate the text data
  (bytes-append (hash-ref inner 'keyword)
                (bytes 0)
                (hash-ref hsh 'compression-method)
                (cond
                  ; we want compression and we're using DEFLATE
                  [(bytes=? (hash-ref hsh 'compression-method) (bytes 0))
                   (define bstr (hash-ref inner 'text))
                   (define uncompressed-in (open-input-bytes bstr))
                   (define uncompressed-out (open-output-bytes))
                   (deflate uncompressed-in uncompressed-out)
                   (define compressed
                     (bytes-append (bytes #x78 #x9c)
                                   (get-output-bytes uncompressed-out)
                                   (num->bstr (bytes-adler32 bstr))))
                   (close-input-port uncompressed-in)
                   (close-output-port uncompressed-out)
                   compressed]
                  ; we have no idea what's going on, just return the text
                  [else (hash-ref inner 'text)])))

; take the itxt-data hash and returns an iTXt byte string
(define/contract (itxt-hash->data hsh)
  (hash? . -> . bytes?)
  (define inner (hash-ref hsh 'data))
  (bytes-append (hash-ref inner 'keyword)
                (bytes 0)
                (hash-ref inner 'compression-flag)
                (hash-ref inner 'compression-method)
                (hash-ref inner 'language-tag)
                (bytes 0)
                (hash-ref inner 'translated-keyword)
                (bytes 0)
                (cond
                  ; we don't want compression, return the text
                  [(bytes=? (hash-ref inner 'compression-flag) (bytes 0))
                   (hash-ref inner 'text)]
                  ; we want compression and we're using DEFLATE
                  [(and (bytes=? (hash-ref inner 'compression-flag) (bytes 1))
                        (bytes=? (hash-ref inner 'compression-method) "\0"))
                   (define bstr (hash-ref inner 'text))
                   (define uncompressed-in (open-input-bytes bstr))
                   (define uncompressed-out (open-output-bytes))
                   (deflate uncompressed-in uncompressed-out)
                   (define compressed
                     (bytes-append (bytes #x78 #x9c)
                                   (get-output-bytes uncompressed-out)
                                   (num->bstr (bytes-adler32 bstr))))
                   (close-input-port uncompressed-in)
                   (close-output-port uncompressed-out)
                   compressed]
                  ; we have no idea what's going on, just return the text
                  [else (hash-ref inner 'text)])))

; reads strings and returns a tEXt chunk byte string
; creates a complete tEXt chunk
(define/contract (make-text-chunk keyword [str ""])
  (->* (string?)
       (string?)
       bytes?)
  (define type #"tEXt")
  (define kw (string->bytes/latin-1 keyword))
  (define bstr (string->bytes/latin-1 str))
  (define info+data
    (bytes-append kw
                  (bytes 0)
                  bstr))
  (with-output-to-bytes
      (λ ()
        (printf "~a~a~a~a"
                (integer->integer-bytes (bytes-length info+data) 4 #f #t)
                type
                info+data
                (integer->integer-bytes (bytes-crc32 (bytes-append type info+data)) 4 #f #t)))))

; reads strings and returns a zTXt chunk byte string with compressed data
; creates a complete zTXt chunk
(define/contract (make-ztxt-chunk keyword [str ""])
  (->* (string?)
       (string?)
       bytes?)
  (define type #"zTXt")
  (define kw (string->bytes/latin-1 keyword))
  (define bstr (string->bytes/latin-1 str))
  (define compression-method (bytes 0))
  (define data
    (let ([data-in (open-input-bytes bstr)]
          [data-out (open-output-bytes)])
      (deflate data-in data-out)
      (define compressed
        (bytes-append (bytes #x78 #x9c)
                      (get-output-bytes data-out)
                      (num->bstr (bytes-adler32 bstr))))
      (close-input-port data-in)
      (close-output-port data-out)
      compressed))
  (define info+data
    (bytes-append kw
                  (bytes 0)
                  compression-method
                  data))
  (with-output-to-bytes
      (λ ()
        (printf "~a~a~a~a"
                (integer->integer-bytes (bytes-length info+data) 4 #f #t)
                type
                info+data
                (integer->integer-bytes (bytes-crc32 (bytes-append type info+data)) 4 #f #t)))))

; reads strings and returns an iTXt chunk byte string with compressed(?) data
; creates a complete iTXt chunk
(define/contract (make-itxt-chunk keyword [str ""] [language-tag ""] [translated-kw ""])
  (->* (string?)
       (string? string? string?)
       bytes?)
  (define type #"iTXt")
  (define kw (string->bytes/utf-8 keyword))
  (define bstr (string->bytes/utf-8 str))
  (define ltag-bstr (string->bytes/utf-8 language-tag))
  (define tkw-bstr (string->bytes/utf-8 translated-kw))
  (define compression-flag (if (>= (bytes-length bstr) 1024)
                               (bytes 1)
                               (bytes 0)))
  (define compression-method (bytes 0))
  (define data
    (cond [(bytes=? compression-flag (bytes 0)) bstr]
          [else
           (define data-in (open-input-bytes bstr))
           (define data-out (open-output-bytes))
           (deflate data-in data-out)
           (define compressed
             (bytes-append (bytes #x78 #x9c)
                           (get-output-bytes data-out)
                           (num->bstr (bytes-adler32 bstr))))
           (close-input-port data-in)
           (close-output-port data-out)
           compressed]))
  (define info+data
    (bytes-append kw
                  (bytes 0)
                  compression-flag
                  compression-method
                  ltag-bstr
                  (bytes 0)
                  tkw-bstr
                  (bytes 0)
                  data))
  (with-output-to-bytes
      (λ ()
        (printf "~a~a~a~a"
                (integer->integer-bytes (bytes-length info+data) 4 #f #t)
                type
                info+data
                (integer->integer-bytes (bytes-crc32 (bytes-append type info+data)) 4 #f #t)))))

; reads a complete tEXt chunk byte string and returns a hash of the chunk
; with a nested hash for the data
; creates a complete tEXt hash
(define/contract (make-text-hash chunk)
  (bytes? . -> . hash?)
  (define len (integer-bytes->integer (subbytes chunk 0 4) #f #t))
  (hash 'type #"tEXt"
        'data (text-data->hash chunk)
        'length len
        'crc32 (subbytes chunk (- (bytes-length chunk) 4))))

; reads a complete zTXt chunk byte string and returns a hash of the chunk
; with a nested hash for the compressed zTXt data
; creates a complete zTXt hash
(define/contract (make-ztxt-hash chunk)
  (bytes? . -> . hash?)
  (define len (integer-bytes->integer (subbytes chunk 0 4) #f #t))
  (hash 'type #"zTXt"
        'data (ztxt-data->hash chunk)
        'length len
        'crc32 (subbytes chunk (- (bytes-length chunk) 4))))

; reads a complete iTXt chunk byte string and returns a hash of the chunk
; with a nested hash for the compressed(?) iTXt data
; creates a complete iTXt hash
(define/contract (make-itxt-hash chunk)
  (bytes? . -> . hash?)
  (define len (integer-bytes->integer (subbytes chunk 0 4) #f #t))
  (hash 'type #"iTXt"
        'data (itxt-data->hash chunk)
        'length len
        'crc32 (subbytes chunk (- (bytes-length chunk) 4))))

; takes a PNG hash and a tEXt hash
; returns a new PNG hash with the tEXt hash added
(define/contract (text-set png-hash text-hash keyword)
  (hash? hash? string? . -> . hash?)
  (cond [(hash-has-key? png-hash 'tEXt)
         (define kw-bstr (string->bytes/latin-1 keyword))
         (define text-lst
           ; if we already have the kw, replace it
           (for/list ([text (in-list (hash-ref png-hash 'tEXt))])
             (define text-data-hash (hash-ref text 'data))
             (if (bytes=? kw-bstr (hash-ref text-data-hash 'keyword))
                 text-hash
                 text)))
         ; make sure the tEXt hash is in the list
         (if (member text-hash text-lst)
             (hash-set png-hash 'tEXt text-lst)
             (hash-set png-hash 'tEXt (append text-lst (list text-hash))))]
        [else (hash-set png-hash 'tEXt (list text-hash))]))

; takes a PNG hash and a zTXt hash
; returns a new PNG hash with the zTXt hash added
(define/contract (ztxt-set png-hash ztxt-hash keyword)
  (hash? hash? string? . -> . hash?)
  (cond [(hash-has-key? png-hash 'zTXt)
         (define kw-bstr (string->bytes/latin-1 keyword))
         (define ztxt-lst
           ; if we already have the kw, replace it
           (for/list ([ztxt (in-list (hash-ref png-hash 'zTXt))])
             (define ztxt-data-hash (hash-ref ztxt 'data))
             (if (bytes=? kw-bstr (hash-ref ztxt-data-hash 'keyword))
                 ztxt-hash
                 ztxt)))
         ; make sure the zTXt hash is in the list
         (if (member ztxt-hash ztxt-lst)
             (hash-set png-hash 'zTXt ztxt-lst)
             (hash-set png-hash 'zTXt (append ztxt-lst (list ztxt-hash))))]
        [else (hash-set png-hash 'zTXt (list ztxt-hash))]))

; takes a PNG hash and an iTXt hash
; returns a new PNG hash with the iTXt hash added
(define/contract (itxt-set png-hash itxt-hash keyword)
  (hash? hash? string? . -> . hash?)
  (cond [(hash-has-key? png-hash 'iTXt)
         (define kw-bstr (string->bytes/utf-8 keyword))
         (define itxt-lst
           ; if we already have the kw, replace it
           (for/list ([itxt (in-list (hash-ref png-hash 'iTXt))])
             (define itxt-data-hash (hash-ref itxt 'data))
             (if (bytes=? kw-bstr (hash-ref itxt-data-hash 'keyword))
                 itxt-hash
                 itxt)))
         ; make sure the iTXt hash is in the list
         (if (member itxt-hash itxt-lst)
             (hash-set png-hash 'iTXt itxt-lst)
             (hash-set png-hash 'iTXt (append itxt-lst (list itxt-hash))))]
        [else (hash-set png-hash 'iTXt (list itxt-hash))]))
