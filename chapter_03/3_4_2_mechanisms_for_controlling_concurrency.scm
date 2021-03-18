;; 3.4.2 Mechanisms for Controlling Concurrency

;; Exercise 3.39: Possible values for different serializers

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
;; 100, 11, 121, 101, 110

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))
;; 101 or 121

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))
;; 101, 121, 100, 11

;; Exercise 3.40: Give all possible values of x

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))
;; 100, 1000, 10000, 100000, 1000000

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
;; 1000000
