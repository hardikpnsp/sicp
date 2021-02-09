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
