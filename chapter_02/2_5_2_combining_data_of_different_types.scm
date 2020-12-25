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
                       (list op type-tags)))))))
