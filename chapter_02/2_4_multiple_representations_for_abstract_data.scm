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

;; Exercise 2.74:  Insatiable Enterprises

#|

- Each division contains a file with set of records keyed on employee names
- structure of set varies from division to division
- each record is itself a set
- - - (<identifier> <value>)
- Example data
((employee_two_name (address address_value) (salary 100000)) 
 (employee_one_name (address address_value) (salary 100000)))

|#

;; a. get-record

(define (get-record employee tagged-division-file)
  ((get 'get-record (type-tag tagged-division-file))
   employee
   (contents tagged-division-file)))

;; each division file should contain its tag so that we can fetch appropriate method for get-record via dynamic dispatch

;; b. get-salary

(define (get-salary tagged-record)
  ((get 'get-salary (tag-type tagged-record))
   (contents tag-record)))

;; c. find-employee-record

(define (find-employee-record employee division-files-list)
  (if (null? difision-file-list)
      #f
      (let ((tagged-file (car division-files-list)))
        (let ((data (get-record employee tagged-file)))
          (if (null? data)
              (find-employee-record employee (cdr division-files-list))
              data)))))

;; d. adding data to lookup table

(put 'get-record 'new-company-tag-for-division-file method-get-record)
(put 'get-salary 'new-company-tag-for-division-file method-get-salary)

;; Exercise 2.75: make-from-mag-ang in message passing style

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part)
           (* x (cos y)))
          ((eq? op 'imag-part)
           (* x (sin y)))
          (else (error "Unknown op --- MAKE-FROM-MAG-ANG"))))
  dispatch)

;; Exercise 2.76: large system with generic operations

#|

- Three strategies

1. generic operations with explicit dispatch

## Changes that must be made for adding new type
- Every individual implementation has to handle new type on their own.

## Changes that must be made for adding new operation
- adding new operation can be done without altering existing code
- have to implement one new method for the new operation which handles all type scenario for the new op

2. data-directed style

## Changes that must be made for adding new type
- New type and its handling method must be intalled in the dispatch table 
- have to add one more column in the dispatch table

## Changes that must be made for adding new operation
- Adding new operation can be done without changing existing code
- have to add a row in the dispatch table and implementation method for each type

3. message-passing style 

## Changes that must be made for adding new type
- adding new type can be done without changing existing code

## Changes that must be made for adding new operation
- Adding new operation means we have to change each dispatch method to handle the new operation


|#
