#| -> Sets as unordered lists |#

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

#| -> Sets as ordered lists |#

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; Exercise 2.61: adjoin-set

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) (cdr set))
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(adjoin-set 3 (list 1 2 4 5 6))

;; here adjoin-set only has to iterate till x < (car set).
;; it is on average about half compared to element-of-set? 

;; Exercise 2.62: union-set O(n)

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        ((= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) (cdr set2))))
        (else (cons (car set2)
                    (union-set set1 (cdr set2))))))

(union-set (list 1 3 5) (list 2 4 6))
(union-set (list 2 4 6) (list 1 2 3 4 5))
