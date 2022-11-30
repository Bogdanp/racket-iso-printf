#lang racket/base

;; https://man.openbsd.org/OpenBSD-3.3/cat3/sprintf.0

(require racket/fixnum
         racket/format
         racket/math)

(provide
 current-custom-conversions
 (struct-out directive)
 parse-directive

 flag-alt?
 flag-zero?
 flag-ladj?
 flag-sign?
 flag-space?
 flag-group?
 flag-short?
 flag-long?
 flag-quad?
 flag-double?
 mask-size)

(define current-custom-conversions
  (make-parameter (hasheqv)))

(define flag-alt?    #b0000000001)
(define flag-zero?   #b0000000010)
(define flag-ladj?   #b0000000100)
(define flag-sign?   #b0000001000)
(define flag-space?  #b0000010000)
(define flag-group?  #b0000100000)
(define flag-short?  #b0001000000)
(define flag-long?   #b0010000000)
(define flag-quad?   #b0100000000)
(define flag-double? #b1000000000) ;; unused; all reals are treated as doubles
(define mask-size    #b1111000000)

(struct directive (arg flags width precision formatter default consume?))

(define (parse-directive who s [start 0])
  (let loop ([arg #f] [flags 0] [width #f] [precision #f] [idx start])
    (define c
      (string-ref* s idx))
    (case c
      ;; flags
      [(#\# #\0 #\- #\space #\+ #\')
       (when (or width precision)
         (error who "flags cannot appear after width or precision"))
       (define new-flags
         (fxior flags (case c
                        [(#\#)     flag-alt?]
                        [(#\0)     flag-zero?]
                        [(#\-)     flag-ladj?]
                        [(#\space) flag-space?]
                        [(#\+)     flag-sign?]
                        [(#\')     flag-group?])))
       (loop arg new-flags #f #f (add1 idx))]

      ;; arg or width
      [(#\*)
       (loop arg flags '* precision (add1 idx))]

      [(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (when width
         (error who "cannot specify width twice"))
       (when precision
         (error who "cannot specifiy width after precision"))
       (define-values (new-width next-idx)
         (parse-width s idx))
       (case (string-ref* s next-idx)
         [(#\$)
          (when arg
            (error who "cannot specify arg twice"))
          (loop (sub1 new-width) flags #f #f (add1 next-idx))]
         [else
          (loop arg flags new-width #f next-idx)])]

      ;; precision
      [(#\.)
       (when precision
         (error who "cannot specify precision twice"))
       (case (string-ref* s (add1 idx))
         [(#\*)
          (loop arg flags width '* (+ idx 2))]

         [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (define-values (new-precision next-idx)
            (parse-width s (add1 idx)))
          (loop arg flags width new-precision next-idx)]

         [else
          (loop arg flags width 0 (add1 idx))])]

      ;; size
      [(#\h #\l #\L)
       (unless (or (and (char=? c #\l) (zero? (fxand flags flag-quad?)))
                   (zero? (fxand flags mask-size)))
         (error who "cannot specify multiple size flags"))
       (define new-flags
         (fxior flags (case c
                        [(#\h) flag-short?]
                        [(#\l) (if (zero? (fxand flags flag-long?))
                                   flag-long?
                                   flag-quad?)]
                        [(#\L) flag-double?])))
       (loop arg new-flags width precision (add1 idx))]

      ;; conversions
      [(#\d #\i #\o #\u #\x #\X)
       (let ([flags (if precision (fxand flags (fxnot flag-zero?)) flags)])
         (define-values (signed? upcase? base)
           (case c
             [(#\o) (values #f #f 8)]
             [(#\u) (values #f #f 10)]
             [(#\x) (values #f #f 16)]
             [(#\X) (values #f #t 16)]
             [else  (values #t #f 10)]))
         (define (formatter precision n)
           (let ([n (if (exact-integer? n) n 0)])
             (define resized-n
               (cond
                 [(on? flags flag-quad?)  (reinterpret (bitwise-and n #xFFFFFFFFFFFFFFFF) 8 signed?)]
                 [(on? flags flag-long?)  (reinterpret (bitwise-and n #xFFFFFFFF)         4 signed?)]
                 [(on? flags flag-short?) (reinterpret (bitwise-and n #xFFFF)             2 signed?)]
                 [(not signed?)           (reinterpret (bitwise-and n #xFFFFFFFF)         4 #f)]
                 [else n]))
             (let* ([prefix (cond
                              [(zero? n) ""]
                              [(on? flags flag-alt?)
                               (case c
                                 [(#\o) "0"]
                                 [(#\x) "0x"]
                                 [(#\X) "0X"]
                                 [else  ""])]
                              [else ""])]
                    [prefix (cond
                              [(and signed?
                                    (on? flags flag-sign?)
                                    (not (negative? resized-n)))
                               (string-append "+" prefix)]
                              [(and signed?
                                    (on? flags flag-space?)
                                    (not (negative? resized-n)))
                               (string-append " " prefix)]
                              [else
                               prefix])])
               (let* ([str (number->string resized-n base)]
                      [str (if upcase? (string-upcase str) str)]
                      [str (if (and precision (positive? precision))
                               (~a str #:min-width precision #:align 'right #:pad-string "0")
                               str)])
                 (if (on? flags flag-zero?)
                     (values prefix str)
                     (values "" (string-append prefix str)))))))
         (values (directive arg flags width precision formatter 0 #t) (add1 idx)))]

      [(#\f #\e #\E #\g #\G)
       (let ([precision (or precision 6)])
         (define (formatter precision v)
           (define-values (precision* notation upcase? trailing?)
             (case c
               [(#\g)
                (case (pick-notation precision v)
                  [(exponential) (values precision                      'exponential #f #f)]
                  [(positional)  (values (adjust-precision precision v) 'positional  #f #f)])]
               [(#\G)
                (case (pick-notation precision v)
                  [(exponential) (values precision                      'exponential #t #f)]
                  [(positional)  (values (adjust-precision precision v) 'positional  #t #f)])]
               [(#\e) (values precision 'exponential #f #t)]
               [(#\E) (values precision 'exponential #t #t)]
               [else  (values precision 'positional  #f #t)]))
           (define n (if (real? v) v 0.0))
           (define prefix
             (cond
               [(and (on? flags flag-sign?)  (not (negative? n))) "+"]
               [(and (on? flags flag-space?) (not (negative? n))) " "]
               [else ""]))
           (define str
             (~r n
                 #:notation notation
                 #:precision (cond
                               [(on? flags flag-alt?) `(= ,precision*)]
                               [(zero? precision*) 0]
                               [trailing? `(= ,precision*)]
                               [else precision*])))
           (let ([str (if upcase? (string-upcase str) str)])
             (if (on? flags flag-zero?)
                 (values prefix str)
                 (values "" (string-append prefix str)))))
         (values (directive arg flags width precision formatter 0.0 #t) (add1 idx)))]

      [(#\s)
       (define (formatter precision v)
         (define str
           (cond
             [(string? v) v]
             [(bytes? v) (bytes->string/utf-8 v)]
             [else "%(invalid)"]))
         (if (and precision (positive? precision))
             (values "" (substring str 0 (min (string-length str) precision)))
             (values "" str)))
       (values (directive arg flags width precision formatter "" #t) (add1 idx))]

      [(#\c)
       (define (formatter precision n)
         (cond
           [(exact-integer? n) (values "" (integer->char n))]
           [else (values "" "%(invalid)")]))
       (values (directive arg flags width precision formatter 0 #t) (add1 idx))]

      [(#\p)
       (define (formatter precision v)
         (if (null? v)
             (values "" "(null)")
             (values "0x" (number->string (eq-hash-code v) 16))))
       (values (directive arg flags width precision formatter null #t) (add1 idx))]

      [(#\%)
       (define (formatter _precision _) (values "" "%"))
       (values (directive #f 0 #f #f formatter #f #f) (add1 idx))]

      [(#f)
       (error who "unexpected end of string while parsing directive")]

      [else
       (define custom-proc
         (hash-ref (current-custom-conversions) c #f))
       (unless custom-proc
         (error who "unexpected directive character '~a'" c))
       (values (custom-proc flags arg width precision) (add1 idx))])))

(define (parse-width s [start 0])
  (let loop ([n 0] [idx start])
    (define c
      (string-ref* s idx))
    (case c
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (loop (+ (* n 10) (- (char->integer c) 48)) (add1 idx))]
      [else
       (values n idx)])))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-ref* s idx)
  (and (< idx (string-length s))
       (string-ref s idx)))

(define (on? v flag)
  (fx= (fxand v flag) flag))

(define (reinterpret n size signed?)
  (integer-bytes->integer
   (integer->integer-bytes n size #f)
   signed?))

(define (pick-notation precision n)
  (let ([precision (if (zero? precision) 1 precision)])
    (cond
      [(zero? n) 'positional]
      [(< (abs n) (expt 10 (- precision))) 'exponential]
      [(> (abs n) (expt 10    precision))  'exponential]
      [else 'positional])))

(define (adjust-precision precision n)
  (cond
    [(zero? n) precision]
    [else
     (define leading-count (add1 (exact-round (log (abs n) 10))))
     (max (- precision leading-count) 0)]))
