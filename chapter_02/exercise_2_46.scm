(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
             (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
             (- (ycor-vect vec1) (ycor-vect vec2))))

(define (scale-vect s vec)
  (make-vect (* (xcor-vect vec) s)
             (* (ycor-vect vec) s)))
