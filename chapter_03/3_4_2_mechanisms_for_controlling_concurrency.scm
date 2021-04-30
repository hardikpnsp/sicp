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

;; Exercise 3.46: timing diagram for test-and-set!

#|

following shocases timing diagram (horizontal) of two processes being able to acquire mutex because no atomicity in the test-and-set! procedure

mutex 'acquire -> if (car cell) -> set-car! cell true --
                           |                           |
cell: (false) -------------                           (true)  (true)
                           |                                   |
mutex 'acquire -> if (car cell) ----------> set-car! cell true--

|#

;; Exercise 3.47: A semaphore of size n

;; a. In term of mutexes

(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (total 0))
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (< total n)
                 (begin (set! total (+ total 1))
                        (mutex 'release))
                 (begin (mutex 'release)
                        (semaphore 'acquire))))
            ((eq? m 'release)
             (mutex 'accure)
             (if (total > 0)
                 (set! total (- total 1)))
             (mutex 'release))))
    semaphore))

;; b. In term of atomic test-and-set!

(define (make-semaphore n)
  (let ((cell (list false))
        (total 0))            
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire) ;retry
                 (if (< total n)
                     (begin (set! total (+ 1 total))
                            (clear! cell)))))
            ((eq? m 'release)
             (if (> total 0)
                 (set! total (- total 1)))
             (clear! cell))))
    the-semaphore))
(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))


;; Exercise 3.48: Deadlock-avoidance by smaller digit first

#|

In this mehod, both the processes will always try to grab lock on smaller "digit" account first

process1: a1 -> a2
process2: a2 -> a1

these two processes can result in deadlock when process1 has lock on a1 and process2 on a2 but both process need locks on both resources in order to complete

when we follow smaller "digit" first stratege (or any specific order) 

process1: a1 -> a2
process2: a1 -> a2

each process will try to grab lock on a1 first, if lock on a1 is not available than a2 is not attempted. Won't be resulting in deadlock.

|#


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id))
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))

;; Exercise 3.49: scenario where above strategy doesn't work

#| a far fetched idea,
memory pointer exchange: A B C are memory location pointing to other memory locations
A -> B
B -> A
C

Task 1: We want whatever location A points to (currently B) to point to C... (B -> C)
Parallely
Task 2: We want whatever location B points to (currently A) to point to C... (A -> C)

Here Task 2 gets a lock on B to read where it points (We don't want any writes to memory location when someone else is reading) 
and Task 1 gets a lock on on A... 

-> Deadlock

|#
