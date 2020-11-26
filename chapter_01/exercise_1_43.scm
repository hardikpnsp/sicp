(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i)
    (cond ((= i n) (lambda (x) (f x)))
          (else (compose f (iter (+ 1 i))))))
  (iter 1))

((repeated inc 5) 10)
;; 15


((repeated square 2) 5)
;; 625
