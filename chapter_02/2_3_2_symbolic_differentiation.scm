;; derivation using symbol manipulation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; definition for method used in deriv
(define (variable? x) (symbol? x))
(variable? 'x)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(same-variable? 'x 'x)

(define (make-sum a1 a2) (list '+ a1 a2))
(make-sum 'x 'y)

(define (make-product m1 m2) (list '* m1 m2))
(make-product 'x 'y)

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(sum? '(+ x y))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(product? '(* x y))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; better constructor implementations so that answer is in simplified form

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; Exercise 2.56

#|

d (u^n) / dx = n * u^(n-1) * du/dx

|#

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(exponentiation? '(** x y))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(base '(** x y))
(exponent '(** x y))

(define (make-exponentiation base exp)
  (list '** base exp))
(make-exponentiation 'x 10)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (- (exponent exp)
                                                             1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(** x 3) 'x)

;; (x^2 + 2x)^3 + 30x
(deriv '(+ (** (+ (** x 2) (* x 2)) 3) (* x 30)) 'x)
;; 3 * (x^2 + 2x) * (2x + 2) + 30

;; anything raised to power 0 should be 1 and power 1 should be the thing itself

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 0)
        ((=number? exp 1) base)
        (else (list '** base exp))))



;; Exercise 2.57

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (append (list '+) (cddr s))))
  
(augend (list + 'x 'y 'z))

(define (multiplicand m)
  (if (null? (cdddr m))
      (caddr m)
      (append (list '*) (cddr m))))

(multiplicand (list '* 'x 'y 'z 'a 'b 'c))

(deriv '(* x y (+ x 3)) 'x)
