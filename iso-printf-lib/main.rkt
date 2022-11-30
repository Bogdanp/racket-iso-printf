#lang racket/base

(require racket/contract
         "private/printf.rkt")

(provide
 (contract-out
  [printf (-> string? any/c ... void?)]
  [eprintf (-> string? any/c ... void?)]
  [sprintf (-> string? any/c ... string?)]
  [fprintf (-> output-port? string? any/c ... void?)]))
