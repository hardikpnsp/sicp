; simple recursive process

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

;; at the end they are going to be combination of 0 1 and 2


(define (f n)
  (define (f-i x y z count)
    (cond ((= count n) z)
          (else (f-i (+ x (* 2 y) (* 3 z)) x y (+ count 1)))))
  (f-i 2 1 0 0))
