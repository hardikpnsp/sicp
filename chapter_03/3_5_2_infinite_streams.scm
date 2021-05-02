;; 3.5.2 Infinite Streams

;; Exercise 3.53: describe the elements of the stream

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define s (cons-stream 1 (add-stream s s)))
;; 1 2 4 8 16 32 ...

;; Exercise 3.54: mul-stream

(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-stream ones integers)))

;; (n + 1)! = (n!) * (n + 1)
(define factorials (cons-stream 1 (mul-stream factorials
                                              integers)))

