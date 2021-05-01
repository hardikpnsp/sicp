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
