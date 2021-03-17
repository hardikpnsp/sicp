;; 3.4 Concurrency: Time is of the Essence
;; 3.4.1 The Nature of Time in Concurrent Systems

;; Exercise 3.38: Peter, Paul and Mary

;; Peter
(set! balance (+ balance 10))

;; Paul
(set! balance (- balance 20))

;; Mary
(set! balance (- balance (/ balance 2)))

#|

a) all different possible values if porcesses are sequential in some order 

- There can be 6 different orers

- Peter, Paul, Marry -> 45
- Peter, Marry, Paul -> 35
- Paul, Peter, Marry -> 45
- Paul, Marry, Peter -> 50
- Marry, Peter, Paul -> 40
- Marry, Paul, Peter -> 40 

There can be 4 different values at the end in the bank account -> 35, 40, 45, 50

b) all different possible values if processes are interleaved

- All execute at once  -> balance 100 -> Possible end values: 110, 80, 50
- Peter executes first -> balance 110 -> Possible end values: 90, 55
- Paul executes first  -> balance 80  -> Possible end values: 90, 40
- Marry executes first -> balance 50  -> Possible end values: 60, 30
- Everything follows sequential order (a) -> Possible end values: 35, 40, 45, 50  

Different possible values: 30, 35, 40, 45, 50, 55, 60, 80, 90, 110
(Assumption: when balance - (balance / 2) happens, both balance values will be same) 
|#
