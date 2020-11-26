(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i)
    (cond ((= i n) (lambda (x) (f x)))
          (else (compose f (iter (+ 1 i))))))
  (iter 1))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- (abs v1) (abs v2))) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try start))

(define (average-damp f)
  (lambda (x) (/ (+ x
                    (f x))
                 2)))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 2.0)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y))))
               1.0))

(cube-root 64.0)

(define (4th-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y)))))
               1.0))

(4th-root 16)

(define (5th-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y y)))))
               1.0))


(5th-root 32)


(define (6th-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y y y)))))
               1.0))


(6th-root 2000)

(define (pow x n)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (square x)
    (* x x))
  (define (iter res a n)
    (if (= n 0)
        res
        (if (even? n)
            (iter res (square a) (/ n 2))
            (iter (* res a) a (- n 1)))))
  (iter 1 x n))

(define (nth-root n x)
  (define (log2 x)
    (/ (log x) (log 2)))
  (define (find-avg-damp x)
    (log2 x))
  (fixed-point (
                (repeated average-damp (find-avg-damp n))
                (lambda (y) (/ x (pow y (- n 1)))))
               1.0))

(nth-root 8 (* 2 2 2 2 2 2 2 2))
