(load "exercise_2_17.scm")

(define l (list 1 2 3 4))

(car (last-pair l))

(null? (cdr (list 1)))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(append (list 1 2 3) (list 1 2 3))

(append (list 1 2 3) (car (list 1 )))

(define (but-last l)
  (if (null? (cdr (cdr l)))
      (list (car l))
      (append (list (car l)) (but-last (cdr l)))))

(but-last (list 1 2 3 4 5))

(define (reverse l)
  (define (rec-rev-list l1 l2)
    (if (null? (cdr l1))
        (append l2 l1)
        (rec-rev-list (but-last l1) (append l2 (list (car (last-pair l1)))))))
  (rec-rev-list (but-last l) (list (car (last-pair l)))))

(reverse (list 1 2 3 4 5 6 7))


(define (reverse l)
  (define (iter l1 l2)
    (if (null? l1)
        l2
        (iter (cdr l1) (cons (car l1) l2))))
  (iter l '()))

(reverse (list 1 2 3 4 5 6 7 8))
