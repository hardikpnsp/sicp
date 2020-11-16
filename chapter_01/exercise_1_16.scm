;; compute b^n

(define (fast-exp b n)
  (define (fast-exp-iter b n a)
    (cond ((= n 1) (* b a))
          ((even? n) (fast-exp-iter (* b b) (/ n 2) a))
          (else (fast-exp-iter b (- n 1) (* a b)))))
  (fast-exp-iter b n 1))
