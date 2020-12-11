(define (split placer-one placer-two)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split placer-one placer-two) painter (- n 1))))
          (placer-one painter (placer-two smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
