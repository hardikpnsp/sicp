;; 3.5.2 Infinite Streams

;; Exercise 3.53: describe the elements of the stream

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define s (cons-stream 1 (add-stream s s)))
;; 1 2 4 8 16 32 ...
