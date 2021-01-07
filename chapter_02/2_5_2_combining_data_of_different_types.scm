(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Exercise 2.81: apply-generic may try to coerce same types

#|
- example scenario: apply-generic is called with two scheme-numbers but the operation is not defined.
- We define exp on '(scheme-number scheme-number) but we call exp on '(complex complex)
|#

(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))

;; Louis provides a solution as addtion coercion of same types in the table

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a. when we call exp on two complex numbers

(exp complex-num-1 complex-num-2)

#| 

proc will be false thus apply-generic will try to coerce '(complex complex)

t1->t1 will return complex number complex-num-1 itself thus procedure moves into infinite loop

trace:

1-> (exp cn1 cn2)
2-> (apply-generic 'exp cn1 cn2)
3--> type-tags = (complex complex)
4--> args = (cn1 cn2)
5--> proc = false
6--> t1->t2 = complex->complex  
7--> (apply-generic 'exp (t1->t2 cn1) cn2)
8--> (apply-generic 'exp cn1 cn2)
9---> Same as Step 2 [Infinite recursion]

|#

;; b. Does apply-generic work correctly without same type coersion?
;;
;; Yes, apply-generic works fine without same type coersion, it does not find proc or coersion.

;; c. Modify apply-generic so that it doesn't try coercion if the two arguments have the same type.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags))))))))


;; Exercise 2.82: generalized apply-generic (multiple arguments)

(define (apply-generic op . args)

  (define (coerce type-tags args)
    (let ((coerce-type (find-coerce-type type-tags type-tags)))
      (if coerce-type
          (map (lambda (x, y) (if (eq? coerce-type x)
                                  y
                                  ((get-coercion coerce-type x) y))) type-tags args)
          #f)))

  (define (find-coerce-type coercion-tags to-be-coerced-tags)
    (if (null? coercion-tags)
        #f
        (let ((coercion-tag (car coercion-tags)))
          (if (is-possible-to-coerce? coercion-tag to-be-coerced-tags)
              coercion-tag
              (find-coerce-type (cdr coercion-tags) to-be-coerced-tags)))))
  
  (define (is-possible-to-coerce? coercion-tag to-be-coerced-tags)
    (if (null? to-be-coerced-tags)
        #t
        (let ((to-be-coerced-tag (car to-be-coerced-tags)))
          (if (get-coercion coercion-tag to-be-coerced-tag)
              (is-possible-to-coerce? coercion-tag (cdr to-be-coerced-tags))
              #f))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coercion (coerce type-tags args)))
            (if coercion
                (apply-generic op coercion)
                (error "No operation found for args -- APPLY-GENERIC" op args))))))))

;; This might miss some inter type operations because it coerces everything to one single type
;; For example: suppose there is an operation installed for '(scheme-number complex), it will be missed

;; Exercise 2.83: raises

(define (raise x) (apply-generic 'raise x)) 
(put 'raise 'integer (lambda (x) (make-rational x 1))) 
(put 'raise 'rational (lambda (x) (make-real (/ (numer x) (denom x)))))  
(put 'raise 'real (lambda (x) (make-from-real-imag x 0))) 

;; Exercise 2.84: apply-generic with raises (copied from sicpwiki)

(define (level type) 
(cond ((eq? type 'integer) 0) 
      ((eq? type 'rational) 1) 
      ((eq? type 'real) 2) 
      ((eq? type 'complex) 3) 
      (else (error "Invalid type: LEVEL" type)))) 

(define (apply-generic op . args) 
(let ((type-tags (map type-tag args))) 
  (define (no-method) 
    (error "No method for these types" (list op type-tags))) 
  (let ((proc (get op type-tags))) 
    (if proc 
        (apply proc (map contents args)) 
        (if (not (null? (cdr args))) 
            (let ((raised-args (raise-to-common args))) 
              (if raised-args 
                  (let ((proc (get op (map type-tag raised-args)))) 
                    (if proc 
                        (apply proc (map contents raised-args)) 
                        (no-method))) 
                  (no-method))) 
            (no-method)))))) 

(define (raise-to-common args) 
(let ((raised-args 
       (map (lambda (x) (raise-to-type (highest-type args) x)) 
            args))) 
  (if (all-true? raised-args) 
      raised-args 
      false))) 

(define (all-true? lst) 
(cond ((null? lst) true) 
      ((car lst) (all-true? (cdr lst))) 
      (else false))) 

(define (raise-to-type type item)
(let ((item-type (type-tag item))) 
  (if (eq? item-type type) 
      item 
      (let ((raise-fn (get 'raise item-type))) 
        (if raise-fn 
            (raise-to-type type (raise-fn item)) 
            false)))))

(define (highest-type args) 
(if (null? (cdr args)) 
    (type-tag (car args)) 
    (let ((t1 (type-tag (car args))) 
          (t2 (highest-type (cdr args)))) 
      (let ((l1 (level t1)) (l2 (level t2))) 
        (if (> l1 l2) t1 t2)))))

;; Exercise: 2.85: drop (copied from sicp wiki)

(put 'project 'rational 
     (lambda (x) (make-scheme-number (round (/ (numer x) (denom x)))))) 

;; add into real package 
(put 'project 'real 
     (lambda (x)  
       (let ((rat (rationalize  
                   (inexact->exact x) 1/100))) 
         (make-rational 
          (numerator rat) 
            (denominator rat))))) 

;; add into complex package 
(put 'project 'complex 
     (lambda (x) (make-real (real-part x)))) 

(define (drop x) 
  (let ((project-proc (get 'project (type-tag x)))) 
    (if project-proc 
        (let ((project-number (project-proc (contents x)))) 
          (if (equ? project-number (raise project-number)) 
              (drop project-number) 
              x)) 
        x))) 

;; apply-generic   
(drop (apply proc (map contents args))) 
