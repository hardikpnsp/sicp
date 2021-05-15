;; 3.5.4 Streams and Delayed Evaluation

;; Understanding integration

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-stream ones integers)))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-stream (scale-stream integrand dt)
                             int)))
  int)

(define int-int (integral ones 1 0.1))

(define (display-stream s f e)
  (if (< f e)
      (begin (newline)
             (display (stream-ref s f))
             (display-stream s (+ f 1) e))))

(display-stream int-int 0 10)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-stream (scale-stream integrand dt)
                               int))))
  int)

;; dy/dt = f(y)
;; y = integrate dy

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;; 2.716923...

;; Exercise 3.77: explicit integral definition

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;; 2.716923...

;; Exercise 3.78: 2nd order linear differential equation

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-stream (scale-stream dy a)
                          (scale-stream y b)))
  y)

(stream-ref (solve-2nd 1 0 0.001 1 1) 1000)
;; 2.7169...

;; Exercise 3.79: solve-2nd generalization

(define (solve-2nd-gen f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(stream-ref (solve-2nd-gen (lambda (dy y) (+ (* dy 1) (* y 0))) 0.001 1 1) 1000)
;; 2.7169

;; Exercise 3.80: RLC circuit

(define (RLC R L C dt)
  (define (generate-stream-pair vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (- (/ 1 C))))
    (define dil (add-stream (scale-stream vc (/ 1 L))
                            (scale-stream il (- (/ R L)))))
    (cons vc il))
  generate-stream-pair)

(define stream-pair ((RLC 1 0.2 1 0.1) 0 10))
(define vc-stream (car stream-pair))
(define il-stream (cdr stream-pair))

(display-stream vc-stream 0 10)
#|
0
-1.
-1.5
-1.7
-1.7249999999999999
-1.6524999999999999
-1.5299999999999998
-1.3861249999999998
-1.2376874999999998
-1.0941625
|#

(display-stream il-stream 0 10)
#|
10
5.
2.
.25
-.7250000000000001
-1.225
-1.43875
-1.4843749999999998
-1.4352499999999997
-1.3364687499999997
|#


