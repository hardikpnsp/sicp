;; a 
(define wave 
  (segments->painter (list 
                      ;; ... 
                      (make-segment (make-vect 0.33 0.6) (make-vect 0.44 0.6)))))

;;b
(define (corner-split painter n) 
  (if (= n 0) 
      painter 
      (beside (below painter (up-split painter n)) 
              (below (right-split painter n) (corner-split painter (- n 1)))))) 

;;c
(define (square-limit painter n) 
  (let ((combine4 (square-of-four flip-vert rotate180 
                                  identity flip-horiz))) 
    (combine4 (corner-split painter n)))) 
