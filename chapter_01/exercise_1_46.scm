(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt 2)

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                     (lambda (guess) (average guess (/ x guess)))))

((sqrt 2) 2132.0)

(define (fixed-point f start)
  (define tolerance 0.001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try start))

(fixed-point (lambda (y) (/ (+ y (/ 2 y)) 2)) 1.0)

(define (fixed-point f)
  (iterative-improve (lambda (guess) (< (abs (- guess (f guess))) 0.001))
                     (lambda (guess) (f guess))))

((fixed-point (lambda (y) (/ (+ y (/ 2 y)) 2))) 1.0)
