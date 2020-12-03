(load "exercise_2_12.scm")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-interval 90 110))
(define r2 (make-interval 45 55))

(par1 r1 r2)
(par2 r1 r2)


(define r3 (make-center-percent 100 10))

(define r4 (make-center-percent 50 10))

(par1 r3 r4)
(par2 r3 r4)

(div-interval r1 r2)

(div-interval r1 r1)

(define r5 (make-interval 99 100))
(define r6 (make-interval 49 51))

(div-interval r5 r5)
(div-interval r5 r4)
