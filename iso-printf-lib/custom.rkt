#lang racket/base

(require racket/contract
         "private/directive.rkt")

(provide
 (contract-out
  [current-custom-conversions (parameter/c
                               (hash/c char? (-> exact-integer?
                                                 (or/c #f exact-nonnegative-integer?)
                                                 (or/c #f '* exact-nonnegative-integer?)
                                                 (or/c #f '* exact-nonnegative-integer?)
                                                 directive?)))]
  [struct directive ([arg (or/c #f exact-nonnegative-integer?)]
                     [flags (or/c #f exact-nonnegative-integer?)]
                     [width (or/c #f '* exact-nonnegative-integer?)]
                     [precision (or/c #f '* exact-nonnegative-integer?)]
                     [formatter (-> (or/c #f exact-nonnegative-integer?) any/c (values string? string?))]
                     [default any/c]
                     [consume? boolean?])]))
