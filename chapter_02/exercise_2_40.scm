(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 10)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flat-map (lambda (x) (map (lambda (y) (list x y))
                        (enumerate-interval x n)))
       (enumerate-interval 1 n)))

(unique-pairs 3)

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (prime-sum? x)
  (prime? (+ (car x) (car (cdr x)))))

(define (prime? x)
  (define (miller-rabin-test n)
    (define (iterate x)
      (cond ((= x 0) true)
            ((= (remainder (expmod (+ (random (- n 1)) 1) (- n 1) n) n) 1) (iterate (- x 1)))
            (else false)))
    (if (= n 1) false (iterate 10)))

  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder
                        (expmod (square base) (/ exp 2) m)
                        m))
          (else (remainder
                 (* base (expmod base (- exp 1) m))
                 m))))
  (miller-rabin-test x))

(miller-rabin-test 23)

(prime-sum? (list 1 3))

(define (make-pair-sum x)
  (list (car x) (car (cdr x)) (+ (car x) (car (cdr x)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs 10)
                  
