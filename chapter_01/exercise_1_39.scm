(define (cont-frac-iter n d k)
  (define (iter denominator-addition i)
    (cond ((= i 1) (/ (n i) (+ (d i) denominator-addition)))
          (else (iter (/ (n i) (+ (d i) denominator-addition)) (- i 1)))))
  (iter 0 k))


(define (tan-cf x k)
  ;; ni = (if i = 1 then x else -x^2)
  ;; di = i th odd number
  (cont-frac-iter (lambda (i) (if (= i 1)
                                  x
                                  (- 0 (square x))))
                  (lambda (i) (- (* 2 i) 1))
                  k))


(define pi 3.14159265359)

(tan-cf (/ pi 4) 10.0)

(tan-cf (/ pi 6) 10.0)

(tan-cf (/ pi 2) 10.0)
