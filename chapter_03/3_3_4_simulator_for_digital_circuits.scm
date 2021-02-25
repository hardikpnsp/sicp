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
