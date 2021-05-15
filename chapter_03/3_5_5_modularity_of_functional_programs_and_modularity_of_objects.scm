;; 3.5.5: Modularity of Functional Programs and Modularity of Objects

(define random-init 7)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (remainder (+ (* 7 x) 3) 1729))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

(stream-ref pi 1000)

;; Exercise 3.81: random number generator stream input

(define (rand-gen requests)
  (define s
    (cons-stream
     random-init
     (stream-map (lambda (request prev-value) (cond ((eq? request 'generate)
                                                     (rand-update prev-value))
                                                    ((pair? request)
                                                     (if (eq? (car request) 'reset)
                                                         (cadr request)
                                                         (error "UNKNOWN REQUEST")))
                                                    (else (error "UNKNOWN REQUEST")))
                         )
                 requests s)))
  s)

(define rand-stream (rand-gen (cons-stream
                               'generate
                               (cons-stream
                                'generate
                                (cons-stream
                                 '(reset 7)
                                 (cons-stream
                                  'generate
                                  stream-null))))))

(define (display-stream s f e)
  (if (< f e)
      (begin (newline)
             (display (stream-ref s f))
             (display-stream s (+ f 1) e))))

(display-stream rand-stream 0 5)


;; Exercise 3.82: monte-carlo integration

(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (random range)))) 

(define (random-number-pairs low1 high1 low2 high2) 
  (cons-stream (cons (random-in-range low1 high1) (random-in-range low2 high2)) 
               (random-number-pairs low1 high1 low2 high2))) 

(define (estimate-integral p x1 x2 y1 y2) 
  (let ((area (* (- x2 x1) (- y2 y1))) 
        (randoms (random-number-pairs x1 x2 y1 y2))) 
    (scale-stream (monte-carlo (stream-map p randoms) 0 0) area))) 

(define (sum-of-square x y) (+ (* x x) (* y y))) 
(define f (lambda (x) (not (> (sum-of-square (- (car x) 1) (- (cdr x) 1)) 1)))) 
(define pi-stream (estimate-integral f 0.0 2.0 0.0 2.0))

(stream-ref pi-stream 1000)
;; 3.19...
