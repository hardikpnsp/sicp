(load "chapter_02/exercise_2_33.scm")

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define empty-board '())

(define (get-last positions)
  (if (null? (cdr positions))
      (car positions)
      (get-last (cdr positions))))


(define (get-same positions lp)
  (if (null? (cdr positions))
      true
      (if (= (car positions) lp)
          false
          (get-same (cdr positions) lp))))

(define (safe? k positions)
  (define (get-diag positions lp k)
    (define (iter count pos)
      (if (null? (cdr pos))
          true
          (if (or (= (+ count (car pos)) lp)
                  (= (- (car pos) count) lp))
              false
              (iter (- count 1) (cdr pos)))))
    (iter (- k 1) positions))
  (let ((lp (get-last positions)))
    (and (get-same positions lp)
         (get-diag positions lp k))))

(safe? 8 (list 3 7 2 8 5 1 4 6))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)

