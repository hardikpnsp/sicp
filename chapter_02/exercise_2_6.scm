(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (add-1 one))

(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;; zero -> x -> x
;; one  -> x -> (f x)
;; two  -> x -> (f (f x))

;; following the pattern

;; three-> x -> (f (f (f x)))
;; five -> x -> (f (f (f (f (f x)))))

;; we know two + three -> five
;; if x -> two
;; five -> (f (f (f two)))

;; ((two f) x) -> (f (f x))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(add two three)
(lambda (f) (lambda (x) ((two f) ((three f) x))))
(lambda (f) (lambda (x) ((lambda (x) (f (f x))) ((lambda (x) (f (f (f x)))) x))))
(lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f (f (f x))))))
(lambda (f) (lambda (x) (f (f (f (f (f x)))))))
;; we get five

(define (inc x)
  (+ 1 x))

((zero inc) 2)
((two inc) 2)
(((add two one) inc) 2)
