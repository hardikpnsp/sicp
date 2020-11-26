(define (cont-frac-iter n d k)
  (define (iter denominator-addition i)
    (cond ((= i 1) (/ (n i) (+ (d i) denominator-addition)))
          (else (iter (/ (n i) (+ (d i) denominator-addition)) (- i 1)))))
  (iter 0 k))

;; approximate e - 2 = 0.718281828459045

(cont-frac-iter (lambda (x) 1.0)
                (lambda (x) (cond ((= (- x 2) -1) 1)
                                  ((= (remainder (- x 2) 3) 0) (* 2 (+ (/ (- x 2) 3) 1)))
                                  (else 1)))
                20000)
