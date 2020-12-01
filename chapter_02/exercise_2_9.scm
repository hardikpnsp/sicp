(load "exercise_2_8.scm")

(define (width x)
  (- (upper-bound x) (lower-bound x)))

;; x -> (10 . 20)
(width x)

;; y -> (-10 . 20)
(width y)

(= (width (add-interval x y))
   (+ (width x) (width y)))

(= (width (sub-interval x y))
   (+ (width x) (width y)))


