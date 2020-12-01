(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval x y)
  (cons x y))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define (mul-interval x y)
  (cond ((< (upper-bound x) 0)
         (cond ((< (upper-bound y) 0) (make-interval (* (lower-bound x) (lower-bound y))
                                                     (* (upper-bound x) (upper-bound y))))
               (else (cond ((< (lower-bound y) 0) (make-interval (* (lower-bound x) (upper-bound y))
                                                                 (* (lower-bound x) (lower-bound y))))
                           (else (make-interval (* (lower-bound x) (upper-bound y))
                                                (* (upper-bound x) (lower-bound y))))))))
        (else (cond ((< (upper-bound y) 0)
                     (cond ((< (lower-bound x) 0) (make-interval (* (upper-bound x) (lower-bound y))
                                                                 (* (lower-bound x) (lower-bound y))))
                           (else (make-interval (* (upper-bound x) (lower-bound y))
                                                (* (lower-bound x) (upper-bound y))))))
                    (else
                     (cond ((< (lower-bound x) 0)
                            (cond (
                                   (< (lower-bound y) 0)
                                   (make-interval (min (* (upper-bound x) (lower-bound y))
                                                       (* (upper-bound y) (lower-bound x)))
                                                  (max (* (upper-bound x) (upper-bound y))
                                                       (* (lower-bound x) (lower-bound y)))))
                                  (else
                                   (make-interval (* (lower-bound x) (upper-bound y))
                                                  (* (upper-bound x) (upper-bound y))))))
                           (else
                            (cond ((< (lower-bound y) 0)
                                   (make-interval (* (upper-bound x) (lower-bound y))
                                                  (* (upper-bound y) (upper-bound x))))
                                  (else
                                   (make-interval (* (lower-bound y) (lower-bound x))
                                                  (* (upper-bound y) (upper-bound x))))))))))))

(define x (make-interval -20 10))
(define y (make-interval -10 20))

(mul-interval x y)

