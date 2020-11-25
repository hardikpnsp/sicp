(define (fixed-point f start)
  (define tolerance 0.00000000001)
  (define (close-enough? v1 v2)
    (< (abs (- (abs v1) (abs v2))) tolerance))
  (define (try guess count)
    (newline)
    (display count)
    (display ". Guess: ")
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next (+ count 1)))))
  (try start 0))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(define (average x y)
  (/ (+ x y) 2))

;; average damping
(fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0)

;; new function
;; x ^ x - 1000 = 0
;; x = log(1000) / log(x)
;; without average damping

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
;; takes 66 steps



;; with average damping

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
;; takes 16 steps

;; division by zero :)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.0))
