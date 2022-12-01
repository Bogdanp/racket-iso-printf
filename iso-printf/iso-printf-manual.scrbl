#lang scribble/manual

@(require (for-label iso-printf
                     (except-in racket/base printf eprintf fprintf)
                     racket/contract)
          scribble/examples)

@title{ISO @tt{printf}}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[iso-printf]

This module provides implementations of the ISO standard C @tt{printf}
family of procedures, based on their description in the OpenBSD
manual.

@defproc[(printf [fmt string?] [arg any/c] ...) void?]{
  Prints the @racket[arg]s to standard output according to the format
  string @racket[fmt].

  The @tt{%n} directive is not currently supported.  The @tt{%q}
  directive prints the @racket[eq-hash-code] of a value in hex format.
  Directives typically fail gracefully on invalid or missing input,
  but malformed format strings may raise an error (eg. specifying a
  precision value twice).

  @examples[
    (require iso-printf)
    (printf "hello\n")
    (printf "%03d\n" 1)
    (printf "%04hd %04hd\n" #x7FFF #xFFFFFF)
    (printf "%0*.*f\n" 12 3 1234567.89)
    (printf "%+.9g\n" 1234.456789)
  ]
}

@defproc[(eprintf [fmt string?] [arg any/c] ...) void?]{
  Prints the @racket[arg]s to standard error according to the format
  string @racket[fmt].
}

@defproc[(sprintf [fmt string?] [arg any/c] ...) string?]{
  Constructs a string by interpreting @racket[fmt] against the given
  @racket[arg]s.
}

@defproc[(fprintf [out output-port?] [fmt string?] [arg any/c] ...) void?]{
  Prints the @racket[arg]s to @racket[out] according to the format
  string @racket[fmt].
}
