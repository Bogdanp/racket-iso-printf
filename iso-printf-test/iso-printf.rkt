#lang racket/base

(require iso-printf
         rackunit)

(define-check (check-printf fmt args expected)
  (check-equal? (apply sprintf fmt args) expected))

(define iso-printf-suite
  (test-suite
   "iso-printf"

   (test-suite
    "basics"

    (check-printf "" null "")
    (check-printf "hello" null "hello")
    (check-printf "hello" (list 1 2 3) "hello")
    (check-printf "hello %%" (list 1) "hello %"))

   (test-suite
    "width"

    (check-printf "%0*d" (list 10 5) "0000000005"))

   (test-suite
    "%diouxX"

    (check-printf "hello %d" null "hello 0")
    (check-printf "hello %+5d" (list 128) "hello  +128")
    (check-printf "hello %-+5d" (list 128) "hello +128 ")
    (check-printf "hello %-0+5d" (list 128) "hello +128 ")
    (check-printf "hello %0+5d" (list 128) "hello +0128")
    (check-printf "hello % 3i" (list 1) "hello   1")
    (check-printf "hello % 3i" (list #xFFFFFFFF) "hello  4294967295")
    (check-printf "hello % 3hi" (list #xFFFFFFFF) "hello  -1")
    (check-printf "hello % 3hi" (list #xFFFF0000) "hello   0")
    (check-printf "hello %ld" (list #x7FFFFFFF) "hello 2147483647")
    (check-printf "hello %ld" (list #xFFFFFFFF) "hello -1")
    (check-printf "hello %lld" (list #x7FFFFFFF) "hello 2147483647")
    (check-printf "hello %lld" (list #xFFFFFFFF) "hello 4294967295")
    (check-printf "hello %3o" (list #o777) "hello 777")
    (check-printf "hello %3o" (list -1) "hello 37777777777")
    (check-printf "hello %3llo" (list -1) "hello 1777777777777777777777")
    (check-printf "hello %3x" (list -1) "hello ffffffff")
    (check-printf "hello %3llx" (list -1) "hello ffffffffffffffff")
    (check-printf "hello %#3x" (list -1) "hello 0xffffffff")
    (check-printf "hello %#3llx" (list -1) "hello 0xffffffffffffffff")
    (check-printf "hello %#3X" (list -1) "hello 0XFFFFFFFF")
    (check-printf "hello %#3llX" (list -1) "hello 0XFFFFFFFFFFFFFFFF")
    (check-printf "hello %#08x" (list #xFA) "hello 0x0000fa")
    (check-printf "hello %#8x" (list #xFA) "hello     0xfa")
    (check-printf "hello %015.7d" (list 1) "hello         0000001")
    (check-printf "hello %015.7x" (list 1) "hello         0000001")
    (check-printf "hello %#15.7x" (list 1) "hello       0x0000001")
    (check-printf "hello %#-15.7x" (list 1) "hello 0x0000001      ")
    (check-printf "hello %#-015.7x" (list 1) "hello 0x0000001      ")
    (check-printf "hello %#-015x" (list 1) "hello 0x1            ")
    (check-printf "hello %#x" (list 0) "hello 0")
    (check-printf "hello %#o" (list #o777) "hello 0777")
    (check-printf "hello %#o" (list 0) "hello 0")
    (check-printf "hello % d" (list 5) "hello  5")
    (check-printf "hello % d" (list -5) "hello -5")
    (check-printf "hello %+ d" (list 5) "hello +5")
    (check-printf "hello %+ d" (list -5) "hello -5")
    (check-printf "hello % o" (list #o644) "hello 644")
    (check-printf "hello %04hd" (list #xFFFF) "hello -001"))

   (test-suite
    "%eE"

    (check-printf "hello %e" null "hello 0.000000e+00")
    (check-printf "hello %.2e" null "hello 0.00e+00")
    (check-printf "hello % 10.2e" (list 1) "hello   1.00e+00")
    (check-printf "hello %010.2e" (list 1) "hello 001.00e+00")
    (check-printf "hello %-10.2e" (list 1) "hello 1.00e+00  ")
    (check-printf "hello %-+10.2e" (list 1) "hello +1.00e+00 ")
    (check-printf "hello %-+10.0e" (list 1) "hello +1e+00    ")
    (check-printf "hello %#-+10.0e" (list 1) "hello +1.e+00   ")
    (check-printf "hello %-+10.1e" (list -1.25) "hello -1.3e+00  ")
    (check-printf "hello %E" null "hello 0.000000E+00")
    (check-printf "hello %.2E" null "hello 0.00E+00")
    (check-printf "hello % 10.2E" (list 1) "hello   1.00E+00")
    (check-printf "hello %010.2E" (list 1) "hello 001.00E+00")
    (check-printf "hello %-10.2E" (list 1) "hello 1.00E+00  ")
    (check-printf "hello %-+10.2E" (list 1) "hello +1.00E+00 ")
    (check-printf "hello %-+10.0E" (list 1) "hello +1E+00    ")
    (check-printf "hello %#-+10.0E" (list 1) "hello +1.E+00   ")
    (check-printf "hello %-+10.1E" (list -1.25) "hello -1.3E+00  "))

   (test-suite
    "%f"

    (check-printf "hello %f" null "hello 0.000000")
    (check-printf "hello %.2f" null "hello 0.00")
    (check-printf "hello % 10.2f" (list 1) "hello       1.00")
    (check-printf "hello %010.2f" (list 1) "hello 0000001.00")
    (check-printf "hello %-10.2f" (list 1) "hello 1.00      ")
    (check-printf "hello %-+10.2f" (list 1) "hello +1.00     ")
    (check-printf "hello %-+10.0f" (list 1) "hello +1        ")
    (check-printf "hello %#-+10.0f" (list 1) "hello +1.       ")
    (check-printf "hello %-+10.1f" (list -1.25) "hello -1.3      ")
    (check-printf "%f %5.02f" (list 'a 52) "0.000000 52.00")
    (check-printf "%020.5f" (list -123.456) "-0000000000123.45600"))

   (test-suite
    "%g"

    (check-printf "hello %g" null "hello 0")
    (check-printf "hello %g" (list 10) "hello 10")
    (check-printf "hello %g" (list 12345.678) "hello 12345.7")
    (check-printf "hello %g" (list 123456.789) "hello 123457")
    (check-printf "hello %g" (list 1234567.89) "hello 1.234568e+06")
    (check-printf "hello %G" (list 1234567.89) "hello 1.234568E+06")
    (check-printf "hello %+G" (list 1234567.89) "hello +1.234568E+06")
    (check-printf "hello % 30G" (list 1234567.89) "hello                   1.234568E+06")
    (check-printf "hello % -30g" (list 1234567.89) "hello  1.234568e+06                 ")
    (check-printf "hello %+ 8.*g" (list 2 12.345) "hello      +12")
    (check-printf "hello %+*.*g" (list 12 3 12.345) "hello        +12.3")
    (check-printf "hello %+.9g" (list 1234.456789) "hello +1234.45679")
    (check-printf "%020.5g" (list -123.456) "-0000000000000123.46"))

   (test-suite
    "%c"

    (check-printf "hello %c" (list 65) "hello A")
    (check-printf "hello % 5c there" (list 97) "hello     a there")
    (check-printf "hello % .123c there" (list 97) "hello a there")
    (check-printf "hello %c %c %c" (list 97 98) "hello a b \x00")
    (check-printf "hello %c" (list 'a) "hello %(invalid)c"))

   (test-suite
    "%s"

    (check-printf "hello %s" (list) "hello ")
    (check-printf "hello %s" (list "world") "hello world")
    (check-printf "hello %10s" (list "world") "hello      world")
    (check-printf "hello %-10s" (list "world") "hello world     ")
    (check-printf "hello %5s" (list #"abc") "hello   abc")
    (check-printf "hello %.3s" (list "friend") "hello fri")
    (check-printf "hello %s" (list 123) "hello %(invalid)s"))

   (test-suite
    "%p"

    (check-printf "hello %p" null "hello NULL")
    (check-printf "hello %p" (list null) "hello NULL")
    (check-printf "hello %20p" (list null) "hello                 NULL")
    (check-printf "hello %-20p" (list null) "hello NULL                ")
    (check-true (regexp-match? #rx"^hello 0x([0-9a-f]+)$"
                               (sprintf "hello %p" (list "abc"))))
    (check-true (regexp-match? #rx"^hello  +0x([0-9a-f]+)$"
                               (sprintf "hello %10p" (list 'foo)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests iso-printf-suite))
