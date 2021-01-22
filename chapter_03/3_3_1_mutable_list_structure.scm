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
