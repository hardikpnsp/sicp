(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define m (list (list 1 2 3 4)
                (list 4 5 6 7)
                (list 6 7 8 9)))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3 4) (list 1 2 3 4))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))  m))

(matrix-*-vector m (list 1 2 3 4))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op
                        init
                        (accumulate (lambda (x y) (cons (car x) y))
                                    '()
                                    seqs))
            (accumulate-n op
                          init
                          (accumulate (lambda (x y) (cons (cdr x) y))
                                      '()
                                      seqs)))))
(define mT (list (list 1 4 6)
                 (list 2 5 7)
                 (list 3 6 8)
                 (list 4 7 9)))

(accumulate-n cons '() m)

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (y) (map (lambda (x) (dot-product x y)) n)) m)))

(matrix-*-matrix m m)
