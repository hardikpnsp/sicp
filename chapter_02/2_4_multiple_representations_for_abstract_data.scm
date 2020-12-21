;; Exercise 2.73: Differentiation program

#|

a. (get 'deriv <type>) here returns method with which we can find derivative of (operands exp)

- the exp for numbers? and same-variable? do not have an operator so can't assimilate into the data-directed dispatch.

|#

;; b. derivatives for sums and products and installation in the table

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (install-deriv-sum-multiplication)
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (make-sum x1 x2)
    (cond ((and  (number? x1) (number? x2)) (+ x1 x2))
          ((=number? x1 0) x2)
          ((=number? x2 0) x1)
          (else (list '+ x1 x2))))
  (define (deriv-sum operands)
    (make-sum (deriv (addend operands))
              (deriv (augend operands))))
  (put 'deriv '(+) deriv-sum)

  (define (multiplicant m) (cadr m))
  (define (multiplier m) (caddr m))
  (define (make-product x1 x2)
    (cond ((and (number? x1) (number? x2)) (* x1 x2))
          ((=number? x1 1) x2)
          ((=number? x2 1) x1)
          ((or (=number? x1 0) (=number? x2 0)) 0)
          (else (list '* x1 x2))))
  (define (deriv-product operands)
    (make-sum (make-product (multiplicant operands) (deriv (multiplier operands)))
              (make-product (multiplier operands) (deriv (multiplicant operands)))))
  (put 'deriv '(*) deriv-product))


;; c. install exponentiation

(define (install-deriv-exponents)
  (define (deriv-expoents operands)
    (make-product (make-exponent (base operands)
                                 (- (exponent operands) 1))
                  (make-product (expoenet operands)
                                (deriv (base operands)))))
  (define (make-product x1 x2)
    (cond ((and (number? x1) (number? x2)) (* x1 x2))
          ((=number? x1 1) x2)
          ((=number? x2 1) x1)
          ((or (=number? x1 0) (=number? x2 0)) 0)
          (else (list '* x1 x2))))
  (define (make-exponent base exponent)
    (cond ((and (number? base) (number? exponent)) (expt base exponent))
          ((=number? base 0) 0)
          ((=number? exponent 0) 0)
          ((=number? exponent 1) base)
          ((=number? base 1) 1)
          (else (list '** base exponent))))
  (define (base operands) (cadr operands))
  (define (exponent operands) (caddr operands))
  (put 'deriv '(**) deriv-exponents))

;; d

#|
((get (operator exp) 'deriv) (operands exp) var)

all we have to do is make installation happen in a different order

example:

(put '(**) 'deriv deriv-exponents) 

|#
