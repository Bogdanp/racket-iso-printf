#lang racket/base

(require racket/fixnum
         racket/format
         racket/match
         racket/port
         "directive.rkt")

(provide
 printf
 fprintf
 eprintf
 sprintf)

(define (printf fmt . args)
  (void (interpret-printf-fmt 'printf fmt args write-string)))

(define (eprintf fmt . args)
  (define out (current-error-port))
  (void (interpret-printf-fmt 'eprintf fmt args (λ (s) (write-string s out)))))

(define (sprintf fmt . args)
  (call-with-output-string
   (lambda (out)
     (interpret-printf-fmt 'spritnf fmt args (λ (s) (write-string s out))))))

(define (fprintf out fmt . args)
  (void (interpret-printf-fmt 'fprintf fmt args (λ (s) (write-string s out)))))

(define (interpret-printf-fmt who fmt args handler-proc)
  (define argvec (list->vector args))
  (define (get idx default)
    (if (>= idx (vector-length argvec))
        default
        (vector-ref argvec idx)))
  (let loop ([idx 0] [arg-idx 0])
    (match (regexp-match-positions #rx"%" fmt idx)
      [(list (cons directive-start-idx directive-end-idx))
       (handler-proc (substring fmt idx directive-start-idx))
       (match-define-values ((directive arg flags width precision formatter default consume?) next-idx)
         (parse-directive who fmt directive-end-idx))
       (define-values (actual-width width-arg?)
         (case width
           [(#f) (values 0 #f)]
           [(*)  (values (get arg-idx 0) #t)]
           [else (values width #f)]))
       (define-values (actual-precision precision-arg?)
         (case precision
           [(#f) (values #f #f)]
           [(*)  (values (get (if width-arg? (add1 arg-idx) arg-idx) 0) #t)]
           [else (values precision #f)]))
       (define align-left? (fx= (fxand flags flag-ladj?) flag-ladj?))
       (define pad-zero? (fx= (fxand flags flag-zero?) flag-zero?))
       (let ([arg-idx (cond
                        [(and width-arg? precision-arg?) (+ arg-idx 2)]
                        [(or width-arg? precision-arg?) (add1 arg-idx)]
                        [else arg-idx])])
         (handler-proc
          (let-values ([(p-str str) (formatter actual-precision (get (or arg arg-idx) default))])
            (define padded-str
              (~a str
                  #:min-width (max (- actual-width (string-length p-str)) 0)
                  #:pad-string (if (and pad-zero? (not align-left?)) "0" " ")
                  #:align (if align-left? 'left 'right)))
            (string-append p-str padded-str)))
         (loop next-idx (if consume? (add1 arg-idx) arg-idx)))]
      [#f
       (handler-proc (substring fmt idx))])))
