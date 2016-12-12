#lang racket/base
; txt.rkt
(require file/gunzip
         file/gzip
         file/sha1
         racket/contract
         racket/format
         racket/port
         "base.rkt")
(provide itxt-data->hash
         itxt-hash->data
         make-itxt-chunk
         make-itxt-hash)

; iTXt:
; LENGTH (of compressed data)
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
;                               'compression-mode cmode
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

; reads an iTXt byte string and returns the uncompressed(?) data as a hash
(define/contract (itxt-data->hash bstr)
  (bytes? . -> . hash?)
  (define bstr-in (open-input-bytes bstr))
  (define type (peek-bytes 4 4 bstr-in))
  
  (define kw
    ; loop until we find \0
    (let loop ([offset 8])
      (define byte (peek-bytes 1 offset bstr-in))
      (if (bytes=? byte #"\0")
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
      (cond [(bytes=? byte #"\0")
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
      (cond [(bytes=? byte #"\0")
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
  ; return a hash with all the data information
  (hash
   'keyword kw
   'compression-flag compression-flag
   'compression-method compression-method
   'language-tag language-tag
   'translated-keyword translated-kw
   'text
   (cond [(bytes=? compression-flag #"\1")
          ; inflate the compressed data
          (define compressed-in (open-input-bytes data))
          (define compressed-out (open-output-bytes))
          (inflate compressed-in compressed-out)
          (define uncompressed (get-output-bytes compressed-out))
          (close-input-port compressed-in)
          (close-output-port compressed-out)
          (close-input-port bstr-in)
          uncompressed]
         [else data])))

; take the itxt-data hash and returns an iTXt byte string
(define/contract (itxt-hash->data hsh)
  (hash? . -> . bytes?)
  (define inner (hash-ref hsh 'data))
  (bytes-append (hash-ref inner 'keyword)
                #"\0"
                (hash-ref inner 'compression-flag)
                (hash-ref inner 'compression-method)
                (hash-ref inner 'language-tag)
                #"\0"
                (hash-ref inner 'translated-keyword)
                #"\0"
                (hash-ref inner 'text)))

; reads a string and returns an iTXt chunk byte string with compressed(?) data
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
                               #"\1"
                               #"\0"))
  (define compression-method (bytes 0))
  (define data
    (cond [(bytes=? compression-flag #"\0") bstr]
          [else
           (define data-in (open-input-bytes bstr))
           (define data-out (open-output-bytes))
           (deflate data-in data-out)
           (define compressed (get-output-bytes data-out))
           (close-input-port data-in)
           (close-output-port data-out)
           compressed]))
  (define info+data
    (bytes-append kw
                  #"\0"
                  compression-flag
                  compression-method
                  ltag-bstr
                  #"\0"
                  tkw-bstr
                  #"\0"
                  data))
  (define itxt-bstr
    (with-output-to-bytes
        (λ ()
          (printf "~a~a~a~a"
                  (number->bytes (bytes-length info+data))
                  type
                  info+data
                  (number->bytes (bytes-crc32 info+data))))))
  itxt-bstr)

; reads a complete iTXt chunk byte string and returns a hash of the chunk
; with a nested hash for the compressed(?) iTXt data
; creates a complete iTXt hash
(define/contract (make-itxt-hash chunk)
  (bytes? . -> . hash?)
  (define len (bytes->number (subbytes chunk 0 4)))
  (hash 'type #"iTXt"
        'data (itxt-data->hash chunk)
        'length len
        'crc32 (subbytes chunk (- (bytes-length chunk) 4))))
