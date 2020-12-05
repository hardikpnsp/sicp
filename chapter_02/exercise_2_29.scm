(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)

(define (right-branch mobile)
  (car (cdr mobile)))

(define branch-length car)

(define (branch-structure mobile)
  (car (cdr mobile)))

(define x (make-mobile (make-branch 10 (make-mobile (make-branch 20 20)
                                                    (make-branch 30 30)))
                       (make-branch 20 20)))

(right-branch x)


;; b

(define (total-weight mobile)
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (+ (cond ((pair? (branch-structure lb)) (total-weight (branch-structure lb)))
             (else (branch-structure lb)))
       (cond ((pair? (branch-structure rb)) (total-weight (branch-structure rb)))
             (else (branch-structure rb))))))

(total-weight x)

(define y (make-mobile (make-branch 10 (make-mobile (make-branch 10 20)
                                                    (make-branch 20 10)))
                       (make-branch 30 10)))
;; c

(define (balanced? mobile)
  (define (total-weight-branch b)
    (if (pair? (branch-structure b))
        (total-weight (branch-structure b))
        (branch-structure b)))
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (let ((ls (branch-structure lb))
          (rs (branch-structure rb)))
      (and (= (* (total-weight-branch lb) (branch-length lb))
              (* (total-weight-branch rb) (branch-length rb)))
           (if (pair? ls)
               (balanced? ls)
               true)
           (if (pair? rs)
               (balanced? rs)
               true)))))

(balanced? y)
(balanced? x)

;; d

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define left-branch car)

(define (right-branch mobile)
  (cdr mobile))

(define branch-length car)

(define (branch-structure mobile)
  (cdr mobile))
