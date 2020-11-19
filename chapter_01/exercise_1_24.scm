(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (= 0 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define(divides? a b)
  (= (remainder b a) 0))

(define (search-for-prime start count)
  (define (prime-iter x c)
    (cond ((= c count))
          ((timed-prime-test x) (prime-iter (+ x 1) (+ c 1)))
          (else (prime-iter (+ x 1) c))))
  (prime-iter start 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; tests

(search-for-prime 1000 3)

(search-for-prime 10000 3)

(search-for-prime 100000 3)

(search-for-prime 1000000 3)

(search-for-prime 10000000 3)

(search-for-prime 100000000 3)

(search-for-prime 1000000000 3)

(search-for-prime 1000000000000 3)

;; old test

;; 1000000000039 *** .730000000000004
;; 1000000000061 *** .7299999999999898
;; 1000000000063 *** .7400000000000091

;; new test (after skipping even numbers)

;; 1000000000039 *** .45000000000000284
;; 1000000000061 *** .47000000000000597
;; 1000000000063 *** .45999999999999375

;; with fermat's test

;; 1000000000039 *** 0.
;; 1000000000061 *** 0.
;; 1000000000063 *** 0.

(search-for-prime 10000000000000 3)

;; 10000000000037 *** 2.3700000000000045
;; 10000000000051 *** 2.3299999999999983
;; 10000000000099 *** 2.3499999999999943

;; new test (after skipping even numbers)

;; 10000000000037 *** 1.8400000000000034
;; 10000000000051 *** 1.5899999999999963
;; 10000000000099 *** 1.8400000000000034

;; with fermat's test

;; 10000000000037 *** 0.
;; 10000000000051 *** 0.
;; 10000000000099 *** 0.

(/ (+
 (/ 2.37 1.84)
 (/ 2.33 1.59)
 (/ 2.35 1.84)
 (/ 0.73 0.45)
 (/ 0.73 0.47)
 (/ 0.74 0.46))
   6)

;; factor of 1.4691225933489387 change after optimization by ignoring the even numbers

