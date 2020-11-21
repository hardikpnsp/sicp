(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; a: accumulate

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (accumulate + 0 identity a inc b))

(sum 1 10)

(define (product a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (accumulate * 1 identity a inc b))

(product 1 10)

;; b: accumulate recursive

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b)))) 
