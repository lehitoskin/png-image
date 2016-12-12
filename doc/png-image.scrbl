#lang scribble/manual
@; png-image.scrbl
@(require (for-label racket/base racket/contract "../main.rkt"))

@title{@bold{png-image}: Library to view and modify PNG chunks.}
@author{Lehi Toskin}

@defmodule[png-image]{
  @racketmodname[png-image] will allow programmers to dissect
  a PNG image to view its contents separated by its chunks.
}

@defproc[(png? (img (or/c path-string? bytes?))) boolean?]{
  Checks if the given path or byte string is a valid PNG image.
}

@defproc[(png->hash (img png?)) (and/c hash? immutable?)]{
  Given a valid PNG image, returns an immutable @racket[hash] where each key is
  the name of a chunk (which is itself a hash). If a chunk may appear multiple
  times according to the PNG spec, then the chunk information is wrapped inside
  a list of hashes.

  The chunk hashes have the keys @racket['type 'data 'length 'crc].
}

@defproc[(hash->png (hsh hash?)) bytes?]{
  Given a hash, returns a valid PNG image as a byte string. If converted from
  one form to the other, the resulting byte string may not be identical to the
  original bytes due to the ordering of the chunks, but all the chunks should
  inside the final result.
}
