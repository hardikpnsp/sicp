(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 20 30))

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 20 30))

