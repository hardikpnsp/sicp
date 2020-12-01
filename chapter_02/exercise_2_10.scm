(define (div-interval x y)
  (mul-interval x
                (cond ((or (= (upper-bound y) 0)
                           (= (lower-bound y))) (error "devide by zero not supported!"))
                      (else (make-interval (/ 1.0 (upper-bound y))
                                           (/ 1.0 (lower-bound y)))))))

(define (make-interval x y)
  (cons x y))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define x (make-interval 10 20))

(define y (make-interval 0 5))

(div-interval x y)
