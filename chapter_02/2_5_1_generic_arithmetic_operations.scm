;; Exercise 2.77: complex number selectors

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (magnitude z)
  (apply-generic 'magnitude z))

#| trace of (magnitude z)

z -> (tag content)
z -> ('complex ('rectangular (real-part imaginary-part)))

(magnitude z)
-> (apply-generic 'magnitude z)
--> (get 'magnitude '(complex))
--> (apply magnitude (contents z))
---> (magnitude ('rectangular (real imag)))
-----> (apply-generic 'magnitude z-prime)
------> (get 'magnitude '(rectangular))
------> (apply (lambda-to-calculate-magnitude) ((real imag))
-------> returns magnitude
|# 

;; Exercise 2.78: type-tag, attach-tag, contents redefined

;; old def
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; new defs

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

;; Exercise 2.79: equ? for comparison
;; Exercise 2.80: =zero?

(define (install-scheme-number-package)
  ;; ... 
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  'done)

(define (install-rational-number-package)
  ;; ...
  (define (equ? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  (put 'equ? '(rational-number rational-number) equ?)
  (put '=zero? '(rational-number) (lambda (x) (= 0 (numer x))))
  'done)

(define (install-complex-number-packate)
  ;; ...
  (define (equ? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex complex) equ?)
  (define (=zero? x)
    (equ? x (make-from-real-imag 0 0)))
  (put 'zero? '(complex) =zero?)
  'done)

(define (equ? x y)
  (apply-generic 'equ? x y))

