(define (square-tree tree) (tree-map square tree))

(define (tree-map func tree)
  (map (lambda (sub-tree) (cond ((pair? sub-tree) (tree-map func sub-tree))
                                (else (func sub-tree))))
       tree))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
