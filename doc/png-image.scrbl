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

@defproc[(png? (img (or/c path-string? bytes?))) boolean?]{
  Checks if the given path or byte string is a valid PNG image.
}

@defproc[(png->hash (img png?)) (and/c hash? immutable?)]{
  Given a valid PNG image, returns an immutable @racket[hash] where each key is
  the name of a chunk (which is itself a hash). If a chunk may appear multiple
  times according to the PNG spec, then the chunk information is wrapped inside
  a list of hashes. In txt chunks, if the text has been deflated, it will be
  undeflated in this step.

  The chunk hashes have the keys @racket['(type data length crc)].
}

@defproc[(hash->png (hsh hash?)) bytes?]{
  Given a hash, returns a valid PNG image as a byte string. If converted from
  one form to the other, the resulting byte string may not be identical to the
  original bytes due to the ordering of the chunks, but all the chunks should
  inside the final result. If the hash has txt chunks, the text data will be
  deflated if it exceeds 1024 bytes.
}

@defmodule[png-image/txt]{
  @racketmodname[png-image/txt] provides the following procedures.

  iTXt data hashes have the keys @racket['(keyword
                                           compression-flag
                                           compression-mode
                                           language-tag
                                           translated-keyword
                                           text)].
}

@defproc[(itxt-data->hash (bstr bytes?)) hash?]{
  Take an incomplete iTXt chunk that contains only the data and create a hash.
  If the data is longer than 1024 bytes, it will NOT be deflated here.
}

@defproc[(itxt-hash->data (hsh hash?)) bytes?]{
  Take an iTXt chunk hash and return iTXt data.
}

@defproc[(make-itxt-chunk (keyword string?)
                          (str string? "")
                          (language-tag string? "")
                          (translated-kw string? "")) bytes?]{
  Create a complete iTXt chunk with the keyword. If @racket[str] exceeds 1024
  bytes, it will be deflated.
}

@defproc[(make-itxt-hash (chunk bytes?)) hash?]{
  Create a complete iTXt hash from the complete @racket[chunk] bytes. If the
  text from @racket[chunk] is deflated, it will be inflated in this step.
}
