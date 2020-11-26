(define golden-ratio
  (/ (+ 1 (sqrt 5)) 2))

(/ 1 golden-ratio)

;Value: .6180339887498948

(define (cont-frac n d k)
  (define (iter n d i)
    (cond ((= i k) (/ (n k) (d k)))
          (else (/ (n k) (+ (d k) (iter n d (+ i 1)))))))
  (iter n d 1))

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           11)

;;Value: .6180555555555556 (correct till 4 digits)


;; iterative process for cont-frac

(define (cont-frac-iter n d k)
  (define (iter denominator-addition i)
    (cond ((= i 1) (/ (n i) (+ (d i) denominator-addition)))
          (else (iter (/ (n i) (+ (d i) denominator-addition)) (- i 1)))))
  (iter 0 k))

(cont-frac-iter (lambda (x) 1.0)
                (lambda (x) 1.0)
                11)

