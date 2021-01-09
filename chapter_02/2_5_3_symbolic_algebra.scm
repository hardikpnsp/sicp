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

;; Exercise 2.87: install =zero? for polynomials which have coeff as polynomials

(define (install-poly-zero)
  (define (=zero? poly)
    (if (empty-termlist? poly)
        (the-empty-termlist)
        (if (=zero? (coeff (first-term poly)))
            (=zero? (rest-terms poly))
            #f)))
  
  (put 'zero '(polynomial) =zero?))
