;; 3.3.4 A Simulator for Digital Circuits

;; Exercise 3.28: Or gate as a premitive function

(define (or-gate a1 a2 output)
  (define (or-action)
    (let ((new-value (logical-or (get-signal a1)
                         (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'ok)

;; Exercise 3.29: OR gate with AND and NOT

(define (or-gate a1 a2 output)
  (let ((aa1 (make-wire))
        (aa2 (make-wire))
        (naa1 (make-wire))
        (naa2 (make-wire))
        (anaa (make-wire)))
    (and-gate a1 a1 aa1)
    (and-gate a2 a2 aa2)
    (inverter aa1 naa1)
    (inverter aa2 naa2)
    (and-gate naa1 naa2 anaa)
    (inverter anaa output)))

#| The delay in this OR gate implementation is higher as it adds 

2x (delay AND) + 2x (delay NOT)

|#

;; Exercise 3.30: ripple-carry adder delay

(define (ripple-carry-adder ak bk sk c)
  (define (iter a b s c-out)
    (let ((ai (car a))
          (bi (car b))
          (si (car si)))
      (if (null? ai)
          (set-signal! c-out 0)
          (let ((c-in (make-wire)))
            (full-adder ai bi c-in si c-out)
            (iter (cdr a) (cdr b) (cdr s) c-in)))))
  (iter ak bk sk c))

(define (full-adder a b c-in s c-out)
  (let ((x (make-wire))
        (y (make-wire))
        (z (make-wire)))
    (half-adder b c-in x y)
    (half-adder a x s z)
    (or-gate z y c-out)))
      

#|

If we have N full adders in series, then 

delay of ripple-carry = N * delay of full adder
                      = N * (2 * delay of half adder + delay of or-gate)
                      = N * (2 * (delay of or-gate or delay (not-gate + and-gate) which ever is more) + delay of or-gate)

(* N
   (+ (* 2
         (max or-gate-delay
              (+ inverter-delay and-gate-delay)))
      or-gate-delay))
      
|#          

;; Exercise 3.31: no proc initialization in accept-action-procedure!

#|

We initialize the proc on registration to get the signals propogated in initial configuration.
for example: if we have initially 0 -> inverter should have 1 at the start on when first set-signal! occures

In case of half-adder (A B S C)  without initialization: 

- Truth table

A B S C no proc S C
0 0 0 0         0 0
0 1 1 0         0 0
1 0 1 0         0 0
1 1 0 1         0 0

|# 

;; Exercise 3.32: fifo behaviour of the agenda segment queue

#| and-gate 

each input wire (a1 and a2) of and gate has add-action! which calls (after-delay and-gate-delay (lambda () (set-signal! output new-value)))

let's take the following scenario:

at time 1 -> a1 = 1, a2 = 0

at time 2 -> a1 = 0, a2 = 1

if a2 = 1 set-signal came first, this will trigger (lambda () (set-signal! output 1)) after and-gate-delay

then a1 = 0 will trigger (lambda () (set-signal! output 0)) after and-gate-delay

accepted answer => output = 0

if lifo -> output = 1 is the final answer which is wrong

|#
