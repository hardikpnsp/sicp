(load "exercise_2_2.scm")

(define make-rect cons)
(define base car)
(define side cdr)

(define (perimeter rect)
  (* 2 (+ (length (base rect)) (length (side rect)))))

(define (area rect)
  (* (length (base rect)) (length (side rect))))

(define (length segment)
  (sqrt (+ (square (- (x-point (start-segment segment))
                      (x-point (end-segment segment))))
           (square (- (y-point (start-segment segment))
                      (y-point (end-segment segment)))))))

(define r (make-rect (make-segment (make-point 0 0)
                                   (make-point 0 5))
                     (make-segment (make-point 0 0)
                                   (make-point 5 0))))

(area r)
(perimeter r)

(define (make-rect p1 p2 p3)
  (cond ((and (= (x-point p1) (x-point p2)) (= (y-point p2) (y-point p3)))
         (cons (make-segment p1 p2) (make-segment p2 p3)))
        ((and (= (x-point p1) (x-point p3)) (= (y-point p3) (y-point p2)))
         (cons (make-segment p1 p3) (make-segment p3 p2)))
        (else (cons (make-segment p2 p1) (make-segment p1 p3)))))

(define r2 (make-rect (make-point 0 0)
                      (make-point 0 5)
                      (make-point 5 0)))

(area r2)
(perimeter r2)
