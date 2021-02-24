;; 3.3.3 Representing Tables

;; Exercise 3.24: same-key?

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t (make-table equal?))

((t 'insert-proc!) 'math '+ 10)
((t 'insert-proc!) 'math '- 20)
((t 'insert-proc!) 'computer '* 'operator)
((t 'insert-proc!) 'computer '+ 'operator)

((t 'lookup-proc) 'computer '+)
((t 'lookup-proc) 'math '-)

;; Exercise 3.25: n dimension table

;; returns record / subtable from a key and table of records
(define (assoc key records)
  ;;(display key)
  ;;(newline)
  ;;(display records)
  ;;(newline)
  (cond ((null? records) false)
        ((equal? key (caar records)) records)
        (else (assoc key (cdr records)))))


;; recursive lookups
(define (lookup key-list table)
  (let ((key (car key-list))
        (next-keys (cdr key-list)))
    (let ((subtable (assoc key table)))
      (if subtable
          (cond ((null? next-keys) (cdr (car subtable)))
                (else (lookup next-keys (cdr (car subtable)))))
          false))))

(define (insert! key-list value table)
  (let ((key (car key-list))
        (next-keys (cdr key-list)))
    (let ((record (assoc key table)))
      (if record
          (cond ((null? next-keys)
                 (display record)
                 (set-cdr! (car record) value))
                (else
                 (display record)
                 (newline)
                 (set-cdr! (car record)
                           (cons (cons key (make-table)) (cdr (car record))))
                 (insert! next-keys value (cdr (car record)))))
          (cond ((null? next-keys)
                 (set-cdr! table
                           (cons (cons key value) (cdr table))))
                (else (set-cdr! table
                                (cons (cons key (make-table)) (cdr table)))
                      (insert! next-keys value (cdr (car (cdr table))))))))))

(define (make-table) (cons (cons '*table '()) '()))

(define t (make-table))

(display t)

(insert! (list 'foo) 'bar t)

(insert! (list 'bar) 'foo t)

(insert! (list 'f 'f) 'b t)

(insert! (list 'foo 'foo) 'bar t)

(insert! (list 'f 'f 'f) 'b t)

(lookup (list 'foo) t)

(lookup (list 'foo 'foo) t)

(lookup (list 'f 'f 'f) t)

;; 3.26 binary tree table

#| 

each record will have ((key value) left-child right-child) 

|#

(define (make-tree key-val left right) 
   (list key-val left right)) 

(define (adjoin-set x set) 
   (cond ((null? set) (make-tree x '() '())) 
         ((= (car x) (car (car set))) set) 
         ((< (car x) (car (car set))) 
          (make-tree (car set) 
                     (adjoin-set x (cadr set)) 
                     (caddr set))) 
         ((> (car x) (car (car set))) 
          (make-tree (car set) 
                     (cadr set) 
                     (adjoin-set x (caddr set)))))) 

(define (make-binary-table) 
   (let ((local-table '())) 
     
     (define (lookup key records) 
       (cond ((null? records) #f) 
             ((= key (car (car records))) (car records)) 
             ((< key (car (car records))) (lookup key (cadr records))) 
             (else (lookup key (caddr records))))) 
      
     (define (insert! key value) 
       (let ((record (lookup key local-table))) 
         (if record 
             (set-cdr! record value) 
             (set! local-table (adjoin-set (cons key value) local-table))))) 
     
     (define (get key) 
       (lookup key local-table)) 
     
     (define (dispatch m) 
       (cond ((eq? m 'get) get) 
             ((eq? m 'insert!) insert!) 
             ((eq? m 'print) local-table) 
             (else (error "Undefined operation -- BINARY-TABLE" m)))) 
     dispatch)) 

(define bt (make-binary-table))
((bt 'insert!) 1 'a)
((bt 'insert!) 0 'b)
((bt 'insert!) 2 'c)
(bt 'print)


;; Exercise 3.27: Memoization

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

#| 
memo-fib computes nth Fibonacci number in number of steps proportional to n.

(mem-fib n) = (mem-fib n - 1) + (mem-fib n - 2)

using memoization it always remembers the last two digits thus this operation becomes O(1)

and to find mem-fib N we have to go through all n numbers once, thus O(n)

|#

(define memo-fib
  (memoize fib))
         
#|

This does not work as we need to call memo-fib inside fib in order to take advantage of memoization

|#


