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

#| Sets as binary trees |#

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; Exercise 2.63

#|
     7
    / \
   3   9
  / \   \
 1   5   11

|#

(define t (make-tree 7
                     (make-tree 3
                                (make-tree 1 '() '())
                                (make-tree 5 '() '()))
                     (make-tree 9
                                '()
                                (make-tree 11 '() '()))))

(display t)

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(tree->list-1 t)
;; (1 3 5 7 9 11)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(tree->list-2 t)
;; (1 3 5 7 9 11)

#|
;; a.
 Do the two procedure produce same results? YES, both are in-order traversal

;; b.
Do the two procedures have the same order of growth? NO

-> 1 uses append which takes O(N) at each level amounting to O(NlogN)
-> 2 has O(N) complexity 

|#

;; Exercise 2.64: list->tree

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))

#| Output

        5
     /     \
    1       9
     \     / \
      3   7   11



|#

(list->tree (list 1 10 2 9 3 8))


#| Output

        2
     /     \
    1       3
     \     / \
      10  9   8

|#


#| 

How does it work? 

At each step it devides the list into three parts: left tree, this entry and right tree.
the division is done by a number: left-size or right-size instead of actual division.
the recursive calls ultimately give a balaned binary tree (without caring about the relations between numbers.

Time complexity: O(N)

At each step there are N/2 + N/2 = N items processed.

|#

;; Exercise 2.65 union-set and intersection-set implementation

(define (union-set set1 set2)
  (define (merge-list l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((= (car l1) (car l2))
           (cons (car l1)
                 (merge-list (cdr l1) (cdr l2))))
          ((< (car l1) (car l2))
           (cons (car l1)
                 (merge-list (cdr l1) l2)))
          (else (cons (car l2)
                      (merge-list l1 (cdr l2))))))
  (list->tree (merge-list (tree->list-2 set1)
                          (tree->list-2 set2))))

(tree->list-1 (union-set (list->tree (list 1 3 5 7 9 11))
                         (list->tree (list 2 4 6 8 10))))


(define (intersection-set set1 set2)
  (define (merge-list l1 l2)
    (cond ((or (null? l1) (null? l2))
           '())
          ((= (car l1) (car l2))
           (cons (car l1)
                 (merge-list (cdr l1) (cdr l2))))
          ((< (car l1) (car l2))
           (merge-list (cdr l1) l2))
          (else (merge-list l1 (cdr l2)))))
  (list->tree (merge-list (tree->list-2 set1)
                          (tree->list-2 set2))))

(tree->list-1 (intersection-set (list->tree (list 1 3 5 6 7 9 10))
                                (list->tree (list 2 4 5 6 7 8 10))))
               
