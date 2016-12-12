#lang setup/infotab

(define name "png-image")
(define scribblings '(("doc/manual.scrbl" ())))

(define blurb '("Library to view and modify PNG chunks."))
(define primary-file "main.rkt")

(define required-core-version "6.0")

(define deps '("base" "scribble-lib"))
(define build-deps '("racket-doc"))
