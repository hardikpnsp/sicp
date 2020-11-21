(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value term a next b predicate?)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate? a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                      (expmod (square base) (/ exp 2) m)
                      m))
        (else (remainder
               (* base (expmod base (- exp 1) m))
               m))))

(define (miller-rabin-test n)
  (define (iterate x)
    (cond ((= x 0) true)
          ((= (remainder (expmod (+ (random (- n 1)) 1) (- n 1) n) n) 1) (iterate (- x 1)))
          (else false)))
  (if (= n 1) false (iterate 10)))

(define (sum-of-prime a b)
  (define (prime? x)
    (miller-rabin-test x))
  (define (identity x)
    x)
  (define (inc x)
    (+ 1 x))
  (filtered-accumulate + 0 identity a inc b prime?))

;; sum of prime numbers between 1 and 50

(sum-of-prime 1 50)
(+ 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

(define (sum-of-square-of-prime a b)
  (define (prime? x)
    (miller-rabin-test x))
  (define (inc x) (+ x 1))
  (define (square x)
    (* x x))
  (filtered-accumulate + 0 square a inc b prime?))

;; sum of square of prime numbers between 1 and 10

(sum-of-square-of-prime 1 10)
(+ (square 2) (square 3) (square 5) (square 7))

(define (product-of-relative-prime n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (define (gcd x n)
    (cond ((= x 0) n)
          (else (gcd (remainder n x) x))))
  (define (inc x) (+ 1 x))
  (define (identity x) x)
  (filtered-accumulate * 1 identity 1 inc n relative-prime?))

;; product of relative prime of 10 less than 10

(product-of-relative-prime 10)
(* 3 7 9)
