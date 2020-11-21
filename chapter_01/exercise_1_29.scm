(define (sum term a next b)
  (cond ((> a b) 0)
        (else (+ (term a)
                 (sum term (next a) next b)))))

(define (cube-sum a b)
  (sum cube a inc b))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(cube-sum 1 3)

(define (integral-of function limit-a limit-b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum function (+ limit-a (/ dx 2.0)) add-dx limit-b)
     dx))

(integral-of cube 0 1 0.01)

;; simpson's rule

(define (simpson-integral f a b n)
  (define (get-h)
    (/ (- b a) n))
  (define (get-x k)
    (+ a (* k (get-h))))
  (define (get-c k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (get-y k)
    (f (get-x k)))
  (define (term k)
    (* (get-c k)
       (get-y k)))
  (define (next k)
    (+ 1 k))
  (* (/ (get-h) 3)
     (sum term 0 next n)))

(simpson-integral cube 0 1 1000)
