(define (non-trivial-roots? n m)
  (cond ((= n 1) false)
        ((= n (- m 1)) false)
        (else (= (remainder (square n) m) 1))))

(define (square x)
  (* x x))

(define (miller-rabin-test n)
  (define (iterate x)
    (cond ((= x 0) true)
          ((= (remainder (expmod (+ (random (- n 1)) 1) (- n 1) n) n) 1) (iterate (- x 1)))
          (else false)))
  (iterate 10))

(define (expmod base exp mod)
  (define (square-mod-or-zero n)
    (cond ((non-trivial-roots? n mod) 0)
          (else (remainder (square n) mod))))
  (cond ((= exp 0) 1)
        ((even? exp) (square-mod-or-zero (expmod base (/ exp 2) mod)))
        (else (remainder (* (expmod base (- exp 1) mod) base) mod))))


(miller-rabin-test 561)
(miller-rabin-test 1105)
(define (non-trivial-roots? n m)
  (cond ((= n 1) false)
        ((= n (- m 1)) false)
        (else (= (remainder (square n) m) 1))))

(define (square x)
  (* x x))

(define (miller-rabin-test n)
  (define (iterate x)
    (cond ((= x 0) true)
          ((= (remainder (expmod (+ (random (- n 1)) 1) (- n 1) n) n) 1) (iterate (- x 1)))
          (else false)))
  (iterate 10))

(define (expmod base exp mod)
  (define (square-mod-or-zero n)
    (cond ((non-trivial-roots? n mod) 0)
          (else (remainder (square n) mod))))
  (cond ((= exp 0) 1)
        ((even? exp) (square-mod-or-zero (expmod base (/ exp 2) mod)))
        (else (remainder (* (expmod base (- exp 1) mod) base) mod))))


(miller-rabin-test 561)
(miller-rabin-test 1105)
(define (non-trivial-roots? n m)
  (cond ((= n 1) false)
        ((= n (- m 1)) false)
        (else (= (remainder (square n) m) 1))))

(define (square x)
  (* x x))

(define (miller-rabin-test n)
  (define (iterate x)
    (cond ((= x 0) true)
          ((= (remainder (expmod (+ (random (- n 1)) 1) (- n 1) n) n) 1) (iterate (- x 1)))
          (else false)))
  (iterate 10))

(define (expmod base exp mod)
  (define (square-mod-or-zero n)
    (cond ((non-trivial-roots? n mod) 0)
          (else (remainder (square n) mod))))
  (cond ((= exp 0) 1)
        ((even? exp) (square-mod-or-zero (expmod base (/ exp 2) mod)))
        (else (remainder (* (expmod base (- exp 1) mod) base) mod))))


;; Carmichael numbers does not fool miller-rabin test

(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)

;; normal tests
(miller-rabin-test 1000)
(miller-rabin-test 131)
(miller-rabin-test 1000000009)
