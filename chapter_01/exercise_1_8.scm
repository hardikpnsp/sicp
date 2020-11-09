(define (cube-root-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (cube-root-iter guess (improve guess x)
                 x)))

(define (improve guess x)
  (/ 
    (+ (/ x (* guess guess)) (* 2 guess))
    3))

(define (good-enough? old-guess guess)
  (<
    (abs
      (- old-guess guess))
    (* 0.01 old-guess)))

(define (qr x)
  (cube-root-iter 10.0 1.0 x))


