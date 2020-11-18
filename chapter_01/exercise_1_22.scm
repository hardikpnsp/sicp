(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      (= 0 1)))

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
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define(divides? a b)
  (= (remainder b a) 0))

(define (search-for-prime start count)
  (define (prime-iter x c)
    (cond ((= c count))
          ((timed-prime-test x) (prime-iter (+ x 1) (+ c 1)))
          (else (prime-iter (+ x 1) c))))
  (prime-iter start 0))

;; tests

(search-for-prime 1000 3)

(search-for-prime 10000 3)

(search-for-prime 100000 3)

(search-for-prime 1000000 3)

(search-for-prime 10000000 3)

(search-for-prime 100000000 3)

(search-for-prime 1000000000 3)

(search-for-prime 1000000000000 3)

;; 1000000000039 *** .730000000000004
;; 1000000000061 *** .7299999999999898
;; 1000000000063 *** .7400000000000091

(search-for-prime 10000000000000 3)

;; 10000000000037 *** 2.3700000000000045
;; 10000000000051 *** 2.3299999999999983
;; 10000000000099 *** 2.3499999999999943

;; 10x the number 3x the time
