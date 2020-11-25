(define (fixed-point f start)
  (define tolerance 0.00000000001)
  (define (close-enough? v1 v2)
    (< (abs (- (abs v1) (abs v2))) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try start))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; golden ratio
(/ (+ 1 (sqrt 5)) 2)

