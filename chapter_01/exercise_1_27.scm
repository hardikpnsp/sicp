
;; return true if all number < n pass fermat test (Should mean its a prime number)

(define (fermat-test-all n)
  (define (try-all a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (try-all (+ 1 a)))
          (else false)))
  (try-all 2)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                      (expmod (square base) (/ exp 2) m)
                      m))
        (else (remainder
               (* base (expmod base (- exp 1) m))
               m))))

;; test

(fermat-test-all 131)
;; returns true -> 131 is prime

;; 561 not a prime number, It is one of the carmichael numbers
;; 11 is one of the factors of 561
(/ 561 11)

(fermat-test-all 561)
;; returns true -> fools the fermat test

;; Other Carmichael numbers: 561, 1105, 1729, 2465, 2821, 6601

(fermat-test-all 1105)
(fermat-test-all 1729)
(fermat-test-all 2465)
(fermat-test-all 2821)
(fermat-test-all 6601)

