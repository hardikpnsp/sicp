(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i)
    (cond ((= i n) (lambda (x) (f x)))
          (else (compose f (iter (+ 1 i))))))
  (iter 1))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f x)
          (f (- x dx))
          (f (+ x dx)))
       3)))


;; smoothing of a function: x = 1 if x > 0 else x = 0
(((repeated smooth 3) (lambda (x) (if (> x 0)
                                      1.0
                                      0.0))) 0.00002)
