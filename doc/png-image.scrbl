#lang scribble/manual
@; png-image.scrbl
@(require (for-label racket/base racket/contract "../main.rkt"))

@title{@bold{png-image}: Library to view and modify PNG chunks.}
@author{Lehi Toskin}

@defmodule[png-image]{
  @racketmodname[png-image] will allow programmers to dissect
  a PNG image to view its contents separated by its chunks. This module
  provides all of the below procedures, even if they're in separate
  files.
}

@defproc[(png? [img (or/c path-string? bytes?)]) boolean?]{
  Checks if the given path or byte string is a valid PNG image.
}

@defproc[(png->hash [img png?]) (and/c hash? immutable?)]{
  Given a valid PNG image, returns an immutable @racket[hash] where each key is
  the name of a chunk (which is itself a hash). If a chunk may appear multiple
  times according to the PNG spec, then the chunk information is wrapped inside
  a list of hashes. In txt chunks, if the text has been deflated, it will be
  inflated in this step.

  The chunk hashes have the keys @racket['(type data length crc32)].
}

@defproc[(hash->png [hsh hash?]) bytes?]{
  Given a hash, returns a valid PNG image as a byte string. If converted from
  one form to the other, the resulting byte string may not be identical to the
  original bytes due to the ordering of the chunks, but all the chunks should
  be inside the final result. If the hash has iTXt chunks, the text data will
  be deflated if it exceeds 1024 bytes.
}

@defmodule[png-image/txt]{
  @racketmodname[png-image/txt] provides the following procedures.
   
  tEXt data hashes have the keys @racket['(keyword text)].
  
  zTXt data hashes have the keys @racket['(keyword compression-method text)].
  
  iTXt data hashes have the keys @racket['(keyword
                                           compression-flag
                                           compression-method
                                           language-tag
                                           translated-keyword
                                           text)].
  
  The only valid compression-method value is @racket[#"\0"].
}

@deftogether[(@defproc[(text-data->hash [bstr bytes?]) hash?]
              @defproc[(ztxt-data->hash [bstr bytes?]) hash?]
              @defproc[(itxt-data->hash [bstr bytes?]) hash?])]{
  Take an incomplete chunk that contains only the data and create a hash.
  In iTXt, if the data is longer than 1024 bytes, it will NOT be deflated here.
}

@deftogether[(@defproc[(text-hash->data [hsh hash?]) bytes?]
              @defproc[(ztxt-hash->data [hsh hash?]) bytes?]
              @defproc[(itxt-hash->data [hsh hash?]) bytes?])]{
  Take an incomplete chunk hash and return incomplete data.
}

@deftogether[(@defproc[(make-text-chunk [keyword string?] (str string? "")) bytes?]
              @defproc[(make-ztxt-chunk [keyword string?] (str string? "")) bytes?]
              @defproc[(make-itxt-chunk [keyword string?]
                          (str string? "")
                          (language-tag string? "")
                          (translated-kw string? "")) bytes?])]{
  Create a complete chunk with the keyword. In iTXt, if @racket[str] exceeds
  1024 bytes, it will be deflated. In zTXt, @racket[str] will always be
  deflated.
}

@deftogether[(@defproc[(make-text-hash [chunk bytes?]) hash?]
              @defproc[(make-ztxt-hash [chunk bytes?]) hash?]
              @defproc[(make-itxt-hash [chunk bytes?]) hash?])]{
  Create a complete hash from the complete @racket[chunk] bytes. In iTXt, if
  the text from @racket[chunk] is deflated, it will be inflated in this step.
  In zTXt, the text will always be deflated.
}

@defproc[(bytes-crc32 [bstr bytes?]) integer?]{
  Calculates the CRC32 of the byte string and returns a decimal integer.
}

@defproc[(bytes-adler32 [bstr bytes?]) integer?]{
  Calculates the ADLER32 of the byte string and returns a decimal integer.
}
