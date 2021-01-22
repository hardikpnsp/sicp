;; 3.3 Modeling with Mutable Data

;; 3.3.1 Mutable List Structure

;; Exercise 3.12: append!

(define x (list 'a 'b))
(define y (list 'c 'd))

(define z (append x y))

(display z)
;; (a b c d)

(cdr x)
;; (b)

(define w (append! x y))

(display w)
;;(a b c d)

(cdr x)
;; (b c d)

#| 

1) append does not modify x

     
x -> |a||*| -> |b||'()|
y -> |c||*| -> |d||'()| 

z -> |a||*| -> |b||*| -> |c||*| -> |d||'()|

2) append! modifies x

x -> |a||*| -> |b||*|-> y

y -> |c||*| -> |d||'()|

w -> x
|# 

;; Exercise 3.13: make-cycle

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(display z)
;;infinite loop

(last-pair z)
;;infinite loop

#| as the name suggests, there is a cycle present: a -> b -> c -> a ... |#

;; Exercise 3.14: mystery

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; the process reverses a list

(mystery (list 1 2 3 4))
;; (4 3 2 1)

(define v (list 'a 'b 'c 'd))

#| v -> a * -> b * -> c * -> d '() |#

(define w (mystery v))

(display w)
;; (d c b a)

(display v)
;; (a)
