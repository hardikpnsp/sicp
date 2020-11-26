(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- (abs v1) (abs v2))) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try start))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

((cubic 1 2 3) 3)

(newtons-method (cubic 1 2 3) 1)
;; zero ~> -1.276

(newtons-method (cubic 2 3 4) 1)
;; zero -> -1.650
