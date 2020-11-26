(define (double p)
  (lambda (x)
    (p (p x))))

(define (inc x)
  (+ x 1))

(inc 2)

((double inc) 2)

(((double (double double)) inc) 5)
;; 21

