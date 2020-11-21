;; Iteration product

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; factorial

(define (factorial n)
  (define (term x)
    x)
  (define (next x)
    (+ 1 x))
  (product term 1 next n))

;; pi approximation

(define (approx-pi n)
  (define (term k)
    (/ (get-numerator k)
       (get-denominator k)))
  (define (get-numerator k)
    (cond ((even? k) (+ k 2))
          (else (+ k 1))))
  (define (get-denominator k)
    (cond ((even? k) (+ k 1))
          (else (+ k 2))))
  (define (next k)
    (+ k 1))
  (* (product term 1.0 next n) 4.0))

(approx-pi 1000000)

;; recursive product

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
