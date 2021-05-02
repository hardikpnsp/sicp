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


