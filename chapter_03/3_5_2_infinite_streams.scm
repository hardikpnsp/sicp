;; 3.5.2 Infinite Streams

;; Exercise 3.53: describe the elements of the stream

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define s (cons-stream 1 (add-stream s s)))
;; 1 2 4 8 16 32 ...

;; Exercise 3.54: mul-stream

(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-stream ones integers)))

;; (n + 1)! = (n!) * (n + 1)
(define factorials (cons-stream 1 (mul-stream factorials
                                              integers)))

;; Exercise 3.55: paritla-sum

(define (partial-sum s)
  (define partial-sum-stream (cons-stream (stream-car s)
                                          (add-stream (stream-cdr s)
                                                      partial-sum-stream)))
  partial-sum-stream)

(define int-sum (partial-sum integers))

;; Exercise 3.56: all integers with no prime factors other than 2, 3, 5

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

;; Exercise 3.57: number of additions nth Fibonacci based on add-streams procedures

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

#|
without the memo-proc optimization

fib stream (without the memo-proc optimization):

additions   : 0, 1, (+ 0 1), (+ 1 (+ 0 1)), (+ (+ 0 1) (+ 1 (+ 0 1))),

Nth term will require at least fib(n) additions, which is exponential
|#

;; Exercise 3.58: explain

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

#|

(expand 1 7 10)

1, 4, 2, 8, 5, (expand 5 7 10)...

(expand 3 8 10)

3, 7, 5, 0, 0, 0, 0, 0, ...

|#

;; Exercise 3.59:

;; a: integrate-series

(define (integrate-series s)
  (mul-stream s (stream-map (lambda (x) (/ 1 x)) integers)))

;; b: e^x

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(stream-ref exp-series 5)
;; 1/120

(define cosine-series
  (cons-stream 1 (stream-map -
                             (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Exercise 3.60:

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-stream (scale-stream (stream-cdr s2) (stream-car s1))
                           (mul-series (stream-cdr s1) s2))))


(define one (add-stream (mul-series sine-series sine-series)
                        (mul-series cosine-series cosine-series)))

(stream-ref one 0)
;; 1

(stream-ref one 1)
;; 0 ... and so on
