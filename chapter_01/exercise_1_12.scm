;; Code to find y,x element in a pascal triangle

(define (f y x)
  (cond ((or (= x  y) (= x 1)) 1)
        (else (+ (f (- y 1) (- x 1)) (f (- y 1) x)))))


;; Triangle
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1

;; Test cases

(= (f 1 1) 1)
(= (f 2 1) 1)
(= (f 2 2) 1)
(= (f 3 1) 1)
(= (f 3 2) 2)
(= (f 3 3) 1)
(= (f 4 1) 1)
(= (f 4 2) 3)
(= (f 4 3) 3)
(= (f 4 4) 1)
(= (f 5 1) 1)
(= (f 5 2) 4)
(= (f 5 3) 6)
(= (f 5 4) 4)
(= (f 5 5) 1)
