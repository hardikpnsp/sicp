;; compute b * n
;; same solution can be used for exercise 1.18

(define (fast-mul b n)
  (define (double n)
    (* n 2))
  (define (half n)
    (/ n 2))
  (define (fast-mul-iter b n a)
    (cond ((= n 1) (+ b a))
          ((even? n) (fast-mul-iter (double b) (half n) a))
          (else (fast-mul-iter b (- n 1) (+ a b)))))
  (fast-mul-iter b n 0))
