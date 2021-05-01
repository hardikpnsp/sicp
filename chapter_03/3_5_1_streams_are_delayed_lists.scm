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
