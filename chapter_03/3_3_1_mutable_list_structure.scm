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

;; Exercies 3.15: set-to-wow!

(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(display z1)
(display z2)

(set-to-wow! z1)
;; ((wow b) wow b)

(set-to-wow! z2)
;; ((wow b) a b)

#| 

z1 -> * *
      | |
      a * -> b *

z2 -> * * -> a * -> b *
        |
        ---> a * -> b *

z1 -> * *
      | |
      wow * -> b *

(a is replaced by wow)

z2 -> * * -> wow * -> b *
      |
      -----> a * -> b *

(only the car a of z2 was modified) 

|#

;; Exercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

#| 

 a * -> b * -> c '()

|# 
(count-pairs (list 'a 'b 'c))
;; 3

#|

 a * -> * * -> c '()
        |      ^
        --------
|#

(define last-pair (cons 'c '()))
(define c4 (cons 'a (cons last-pair last-pair)))
(count-pairs c4)
;; 4

#|

* * -> * * -> 'c '()
|     ^|       ^
------- -------
|#

(define last-last-pair (cons last-pair last-pair))
(define c7 (cons last-last-pair last-last-pair))
(count-pairs c7)
;; 7

(define loop (cons 'a 'a))
(set-car! loop loop)
;; (count-pairs (cons 'b (cons 'c loop)))
;; infinite loop

;; Exercise 3.17: correct count-pairs

(define (count-pairs x)
  (define (is-not-visited p visited)
    (if (null? visited)
        #t
        (if (eq? p (car visited))
            #f
            (is-not-visited p (cdr visited)))))
  (let ((visited '()))
    (define (count-pairs-iter p)
      (if (not (pair? p))
          0
          (if (is-not-visited p visited)
              (begin (set! visited (cons p visited))
                     (+ (count-pairs-iter (car p))
                        (count-pairs-iter (cdr p))
                        1))
              0)))
    (count-pairs-iter x)))
  
(count-pairs (list 'a 'b 'c))
;; 3

(count-pairs c4)
;; 3

(count-pairs c7)
;; 3

;; Exercise 3.18: cycle detection

(define (detect-cycle x)
  (define (is-not-visited p visited)
    (if (null? visited)
        #t
        (if (eq? p (car visited))
            #f
            (is-not-visited p (cdr visited)))))
  (define (detect-cycle-iter p visited)
    (if (not (pair? p))
        #f
        (if (is-not-visited p visited)
            (or (detect-cycle-iter (car p) (cons p visited))
                (detect-cycle-iter (cdr p) (cons p visited)))
            #t)))
  (detect-cycle-iter x '()))

(detect-cycle (list 'a 'b 'c))
;; #f

(detect-cycle loop)
;; #t

;; Exercise 3.19: cycle detection - constant space (turtle and hare)

(define (detect-cycle x)
  (define (turtle-hare turtle hare)
    (if (eq? turtle hare)
        #t
        (if (or (not (pair? hare)) (not (pair? turtle)))
            #f
            (cond ((pair? (car hare))
                   (or (turtle-hare (car turtle) (car (car hare)))
                       (turtle-hare (car turtle) (cdr (car hare)))
                       (turtle-hare (cdr turtle) (car (car hare)))
                       (turtle-hare (car turtle) (cdr (car hare)))))
                  ((pair? (cdr hare))
                   (or (turtle-hare (car turtle) (car (cdr hare)))
                       (turtle-hare (car turtle) (cdr (cdr hare)))
                       (turtle-hare (cdr turtle) (car (cdr hare)))
                       (turtle-hare (cdr turtle) (cdr (cdr hare)))))
                  (else #f)))))
  (if (pair? (car x))
      (turtle-hare x (car x))
      (if (pair? (cdr x))
          (turtle-hare x (cdr x))
          #f)))

(detect-cycle (list 'a 'b 'c))
;; #f

(detect-cycle loop)
;; #t 
