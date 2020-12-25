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
