(define (square-list-map items)
  (map (lambda (x) (square x)) items))

(square-list-map (list 1 2 3 4 5))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(square-list (list 1 2 3 4 5))
