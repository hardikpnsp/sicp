(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

(eq? '(this is a list) '(this is a list))
(eq? '(this is a list) '(this (is a) list))

(eq? '() '())

(define (equal? l1 l2)
  (or (eq? l1 l2)
      (and
       (pair? l1)
       (pair? l2)
       (equal? (car l1) (car l2))
       (equal? (cdr l1) (cdr l2)))))

(eq? 1 1)
(eq? 1 2)

(equal? (list 1 2 3) (list 1 2))

(equal? 1 1)
(equal? 2 1)
