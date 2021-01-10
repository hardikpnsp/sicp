;; Given

(the-empty-termlist) ;; returns an empty term list
(empty-termlist? termlist) ;; checks if termlist is empty
(first-term termlist) ;; extracts highest order term
(rest-term termlist) ;; extracts termlist of all terms except first-term
(make-term order coeff) ;; constructs term when given the order and coeff 


(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))


(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))


;; Exercise 2.87: install =zero? for polynomials which have coeff as polynomials

(define (install-poly-zero)
  (define (=zero? poly)
    (if (empty-termlist? poly)
        (the-empty-termlist)
        (if (=zero? (coeff (first-term poly)))
            (=zero? (rest-terms poly))
            #f)))
  
  (put 'zero '(polynomial) =zero?))


;; Exercise 2.88: subtraction of polynomials

(define (negate-poly poly)
  (define (get-negative-termlist termlist)
    (if (empty-termlist? termlist)
        (the-empty-termlist)
        (adjoin-term (make-term (order (first-term termlist))
                                (negate (coeff (first-term termlist))))
                     (adjoin-term (get-negative-termlist (rest-term termlist))))))
  (make-poly (variable poly)
             (get-negative-termlist (term-list poly))))

(define (subtract-poly p1 p2)
  (add-poly p1 (negate p2)))

;; defining generic negate operation

(put 'negate 'scheme-number (lambda (x) (tag (- x))))
(put 'negate 'rational (lambda (x) (make-rational (- (numer x)) (denom x))))
;; ... for each numbre system

(put 'negate 'polynomial negate-poly)

;; define generic negate
(define (negate x)
  (apply-generic 'negate x))

(define (sub x y)
  (apply-generic 'add x (negate y)))


;; Exercise 2.89: procedures for dense polynomial representation

;; dense polynomials are representated as a list of coeff: (1 2 0 3 -2 -5)

(define (first-term term-list)
  (make-term (- (length term-list) 1)
             (car term-list)))

;; for adjoin we can only adjoin higher order terms

(define (adjoin term term-list)
  (if (> (order term) (order (first-term term-list)))
      (cons (coeff term) term-list)
      (error "can't adjoin lower order term")))

;; everything else remains same as before but we will need different methods for arithmatic operations

;; Exercise 2.90: different methods for sparse and dense polynomials

#| 

we will need two more tags sparse and dense for term list
we can store in the dispatch table as 'poly-sparse and 'poly-dense and define operation for each

|#

(define (install-poly-sparse-package)
  ;; definations of sparse poly as above
  (put 'add 'poly-sparse add)
  (put 'mul 'poly-sparse mul)
  (put 'adjoin-term 'poly-sparse adjoin-term)
  (put 'first-term 'poly-sparse first-term)
  ;; ...
  )

;; in similar manner we can install poly-dense-package 
  
#|

Another approach can be to keep 'polynomial package as it is.
but make term-list operations generic.

we can have two packages sparse-term-list and dense-term-list
and store different implementations for adjoin, first-term

and define generic operation using apply-generic over sparse-term-list and dense-term-list

|#
