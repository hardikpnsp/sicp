(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define l (list 1 2 3 4))

(last-pair l)
