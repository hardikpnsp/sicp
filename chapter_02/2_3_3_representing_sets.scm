(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 'x '())

;; Exercise 2.59: Implement union-set

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

(union-set '(x y z) '(a b c))
(union-set '(x y) '(a b c x))
(union-set '(x y z) '(x y))

;; Exercise 2.60: Implement set operations where duplicates are allowed

;; same implementation but might take more time as there are duplicates
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


;; becomes o(1)
(define (adjoin-set x set)
  (cons x set))

;; becomes o(n)
(define (union-set set1 set2)
  (append set1 set2))

;; complexity remains same
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; would use this representations where additive procedures are important compared to intersection
