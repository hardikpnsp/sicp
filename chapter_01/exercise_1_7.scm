(define (sqrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (sqrt-iter guess (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old-guess guess)
  (<
    (abs
      (- old-guess guess))
    (* 0.01 old-guess)))

(define (sq x)
  (sqrt-iter 10.0 1.0 x))


