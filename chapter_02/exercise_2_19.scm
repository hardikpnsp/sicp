(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins)) 0)
        (else (+ (cc amount
                      (cdr coins))
                  (cc (- amount
                         (car coins))
                      coins)))))

(cc 100 (list 50 25 10 5 1))

;; order does not matter as its going to check every combination of coin values
