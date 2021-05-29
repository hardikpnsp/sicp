;; Chpater 4 Metalinguistic Abstractions
;; 4.1 The Metacircular Evaluator
;; 4.1.1 The Core of the Evaluator

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
           (extend-environment
            (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; Exercise 4.1:  list-of-values that evaluates operands from left to right and right to left

#| 
In the prior definition of list-of-values, evaluation order depends on lisp implementation
To make it left to right, we can do explicit calculation instead of passing it in cons
|#

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operands exps) env)))
        (cons left
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values exps env)
  (if (no-operands? exp)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operands exps) env)
              right))))
