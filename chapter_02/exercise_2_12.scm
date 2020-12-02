(load "exercise_2_7.scm")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((percentage (/ (* c p) 100)))
    (make-interval (- c percentage) (+ c percentage))))

(make-center-percent 1000 10)

(define (percent i)
  (/ (* (width i) 100) (center i)))

(percent (make-center-percent 1000 10))


