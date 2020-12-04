;; import reverse and append
(load "exercise_2_18.scm")

(define (same-parity x . l)
  (define (iter l result)
    (let ((r (remainder x 2)))
      (if (null? l)
          (reverse result)
          (iter (cdr l) (if (= (remainder (car l) 2) r)
                            (cons (car l) result)
                            result)))))
  (iter l '()))

(same-parity 1 2 3 4 5 6)

(same-parity 2 3 4 5 67 8 9)
