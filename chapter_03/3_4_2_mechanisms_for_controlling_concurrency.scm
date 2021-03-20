;; 3.4.2 Mechanisms for Controlling Concurrency

;; Exercise 3.39: Possible values for different serializers

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
;; 100, 11, 121, 101, 110

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))
;; 101 or 121

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))
;; 101, 121, 100, 11


;; Exercise 3.40: Give all possible values of x

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))
;; 100, 1000, 10000, 100000, 1000000

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
;; 1000000


;; Exercise 3.41: Serialized balance

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;; continued on next page

  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance)))) ; serialized
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

#| 
- Reading will not be anomalous.
|#

;; Exercise 3.42: Reusing serialized methods, any differnece?

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

;; No difference in concurrency


;; Exercise 3.43: exchanges

;; First version
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; Second version - Exposes bank account serializers using message passing
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

#| 
Three accounts a1, a2, a3 -> 10, 20, 30 balance
If exchage porcesses are run sequencially, at any point on two accounts are "exchanged".
This results in at any time, all three account having 10, 20 and 30 balance in any order

In the first case -> Individual accounts are serialized but not the exchange process. Folling can happen

A1         A2         A3   Description

10         20         30   Initial config

10         20    W <- 20   Exchange A2, A3 (W-A3-10) [E1]

10    W <- 10         20   Exchange A1, A2 (W-A2-10) [E2]

10         10    W <- 10   Exchange A1, A3 (W-A3-10) [E3]

10         20 <- D    10   Exchange A2, A3 (D-A2-10) [E1-complete]

10    W <- 10         10   Exchange A1, A2 (W-A2-10) [E4]

20 <- D    10         10   Exchange A1, A2 (D-A1-10) [E2-complete]

30 <- D    10         10   Exchange A1, A3 (D-A1-10) [E3-complete]

40 <- D    10         10   Exchange A1, A2 (D-A2-10) [E4-complete]


Thus, the overall sum will always remain same (60) but the rules of exchanges can be broken.

If transactions are not serialized, this condition gets violated as well.

A1 A2 A3
10 20 30 
10 20 0  Exchange A1, A3 and Exchange A2, A3 (W-A3-10, W-A3-20)
10 0  0  Exchange A2, A3 and Exchange A2, A3 (W-A2-20, D-A2-10)

Here both Deposit and Withdraw happen in an interleaved manner and A2 is set to 0 -> $10 deposit are lost

|#


;; Exercise 3.44: Transfer

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

#| 
If we have deposit and withdraw serialized, we can use this transfer method as the sum in the system remains same
We do not need fancy solution like we did above for exchange as here, we don't need to keep account balances limited to set 10, 20, 30
Transfer system can have account balance 40, 10, 10. Louis is wrong.
|#


;; Exercise 3.45: Louis's "Simple" bank account system

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

#| 

If we have exchange in the same serialize set as deposit or withdraw,
we won't be able to call the deposit method within exchange method.
Thus, serialized-exchange method will never be executed

|#

