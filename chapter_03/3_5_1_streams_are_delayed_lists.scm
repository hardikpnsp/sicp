;; 3.5: Streams
;; 3.5.1: Streams are Delayed Lists

;; Exercise 3.50: generalized stream-map with allows procedures with multiple arguments

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; Exercise 3.51: what will it print

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
;; 1 2 3 4 5
;; Stream is evaluated till 5

(stream-ref x 7)
;; 6 7
;; stream is already evaluated till 5 so no show calls till that
;; 6 and 7 are not evaluated -> show is called on them

;; Exercise 3.52: what's the value of sum

(define sum 0)
;;0

(define (accum x)
  (set! sum (+ x sum))
  sum)
(display sum)
;;0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display sum)
;;1

(define y (stream-filter even? seq))
(display sum)
;;6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(display sum)
;;10

(stream-ref y 7)
(display sum)
;;136

(display-stream z)
(display sum)
;;210

#| 
The responses will definitely be different if (delay <exp>) is implemented without memo-proc.
the stream will be re-evaluated at every stream-ref and display-stream resulting in sum value increasing everytime.
|#
