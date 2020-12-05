;; old reverse
(define (reverse l)
  (define (iter l1 l2)
    (if (null? l1)
        l2
        (iter (cdr l1) (cons (car l1) l2))))
  (iter l '()))


(define x (list (list 1 2) (list 3 4)))
;; ((1 2) (3 4))

(reverse x)
;; ((3 4) (1 2))

(define (deep-reverse l)
  (define (iter l1 l2)
    (if (null? l1)
        l2
        (iter (cdr l1) (cons (cond ((pair? (car l1)) (deep-reverse (car l1)))
                                   (else (car l1)))
                             l2))))
  (iter l '()))

(deep-reverse x)
