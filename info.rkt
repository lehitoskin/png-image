#lang setup/infotab

(define name "png-image")
(define scribblings '(("doc/png-image.scrbl" ())))

(define blurb '("Library to view and modify PNG chunks."))
(define primary-file "main.rkt")

(define required-core-version "6.0")

(define deps '("base" "rackunit-lib" "scribble-lib"))
(define build-deps '("racket-doc"))
