(define (cons a b)
  (* (pow 2 a)
     (pow 3 b)))

(define (pow b e)
  (define (iter b e a)
    (cond ((= e 0) a)
          ((even? e) (iter (square b) (/ e 2) a))
          (else (iter b (- e 1) (* a b)))))
  (iter b e 1))

(cons 2 3)
;;108

(define (car c)
  (define (iter c ans)
    (cond ((= (remainder c 2) 0) (iter (/ c 2) (+ ans 1)))
          (else ans)))
  (iter c 0))

(car 108)
;;2

(define (cdr c)
  (define (iter c ans)
    (cond ((= (remainder c 3) 0) (iter (/ c 3) (+ ans 1)))
          (else ans)))
  (iter c 0))

(cdr 108)
;;3
