(define (square x) 
  (* x x))

(define (sum-of-square x y) 
  (+ (square x) (square y)))

(define (<= x y)
  (or (< x y) (= x y)))

(define (sum-of-square-of-greater-two x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-square y z))
	((and (<= y x) (<= y z)) (sum-of-square x z))
	(else (sum-of-square x y))))

(sum-of-square-of-greater-two 2 3 4)
(sum-of-square-of-greater-two 3 2 4)
(sum-of-square-of-greater-two 4 3 2)
(sum-of-square-of-greater-two 2 2 3)
