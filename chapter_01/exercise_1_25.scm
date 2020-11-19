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

(define (search-for-prime start count)
  (define (prime-iter x c)
    (cond ((= c count))
          ((timed-prime-test x) (prime-iter (+ x 1) (+ c 1)))
          (else (prime-iter (+ x 1) c))))
  (prime-iter start 0))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fast-exp b n)
  (define (fast-exp-iter b n a)
    (cond ((= n 1) (* b a))
          ((even? n) (fast-exp-iter (* b b) (/ n 2) a))
          (else (fast-exp-iter b (- n 1) (* a b)))))
  (fast-exp-iter b n 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        (else (remainder (fast-exp base exp) m))))

;; tests

(search-for-prime 1000 3)

(search-for-prime 10000 3)

(search-for-prime 100000 3)

;; requires huge time because of base^exp where exp value is so huge
