;; without using higher order procedures
(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree tree)
  (map (lambda (element) (cond ((pair? element) (square-tree element))
                               (else (square element))))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
