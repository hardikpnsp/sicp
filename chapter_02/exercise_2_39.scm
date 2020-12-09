(load "exercise_2_38.scm")

(define (reverse sequence)
  (fold-right (lambda (x y) (fold-right cons (list x) y)) '() sequence))

(reverse (list 1 2 3))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse (list 1 2 3))
