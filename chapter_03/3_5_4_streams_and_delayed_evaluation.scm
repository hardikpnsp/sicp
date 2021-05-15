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




