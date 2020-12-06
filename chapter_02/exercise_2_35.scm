(load "exercise_2_33.scm")

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (cond ((pair? x) (+ 1 (count-leaves (cdr x))))
                                     ((null? x) 0)
                                     (else 1)))
                   t)))

(list (list 1 2) (list 3 4) 5 6 '())
(count-leaves (list (list 1 2) (list 3 4) 5 6 '()))
