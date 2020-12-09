(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-triplets n)
  (flat-map (lambda (x) (flat-map (lambda (y) (map (lambda (z) (list x y z))
                                         (enumerate-interval (+ y 1) n)))
                        (enumerate-interval (+ x 1) n)))
       (enumerate-interval 1 n)))

(unique-triplets 5)

(define (three-sum n sum)
  (define (list-sum result l)
    (if (null? l)
        result
        (list-sum (+ result (car l)) (cdr l))))
        
  (filter (lambda (x) (= (list-sum 0 x) sum))
          (unique-triplets n)))

(three-sum 10 15)
