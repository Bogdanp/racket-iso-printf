#lang info

(define collection "iso-printf")
(define deps '("base"
               "iso-printf-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define implies '("iso-printf-lib"))
(define scribblings '(("iso-printf-manual.scrbl")))