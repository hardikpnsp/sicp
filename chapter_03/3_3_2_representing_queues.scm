;; queue operations

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; Exercise 3.21:

(define q1 (make-queue))
(insert-queue! q1 'a)
;; ((a) a)
(insert-queue! q1 'b)
;; ((a b) b)
(delete-queue! q1)
;; ((b) b)
(delete-queue! q1)
;; (() b)

#| 

the rear-pointer still points to b '() pair, 
but as the front pointer points to '() the queue is considered empty

queue is representated as (front-pointer rear-pointer) thus it shows b twice
if we add something else -> it will go through the empty-queue? flow in insert-queue!
|# 

(insert-queue! q1 'a)
;; ((a) a)

;; print function

(define (print-queue q)
  (display (front-ptr q)))

(print-queue q1)



;; Exercise 3.22: queue as procedure with local state

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" front-ptr)
          (car front-ptr)))

    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else (set-cdr! rear-ptr new-pair)
                    (set-rear-ptr! new-pair)
                    front-ptr))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE called with an empty queue"))
            (else (set-front-ptr! (cdr front-ptr))
                  front-ptr)))
    
    (define (dispatch m)
      (cond ((eq? m 'front)
             (front-queue))
            ((eq? m 'insert!)
             insert-queue!)
            ((eq? m 'delete!)
             (delete-queue!))
            ((eq? m 'get)
             front-ptr)
            (else (error "Unsupported operation --DISPATCH" m))))
    
    dispatch))

(define q (make-queue))

((q 'insert!) 'a)
;; (a)
((q 'insert!) 'b)
;; (a b)
((q 'insert!) 'c)
;; (a b c)
(q 'front)
;; a
(q 'get)
;; (a b c)  
(q 'delete!)
;; (b c)
(q 'get)
;; (b c)



;; Exercise 3.23: deque

#|
representation of a single element: (val *) -> (prev next)
representation: (cons value (cons prev-ptr next-ptr))
|#

(define x (cons 'a (cons 'b 'c)))

(set-car! (cdr x) 'd)

(display x)

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-deque?)
      (null? front-ptr))

    (define (set-fp value)
      (set! front-ptr value))

    (define (set-rp value)
      (set! rear-ptr value))

    (define (make-element value prev next)
      (cons value (cons prev next)))
    
    (define (front-insert-deque! value)
      (cond ((empty-deque?)
             (let ((element (make-element value '() '())))
               (set-rp element)
               (set-fp element)))
            (else
             (let ((element (make-element value '() front-ptr)))
               (set-car! (cdr front-ptr) element)
               (set-fp element)))))

    (define (rear-insert-deque! value)
      (cond ((empty-deque?)
             (let ((element (make-element value '() '())))
               (set-rp element)
               (set-fp element)))
            (else
             (let ((element (make-element value rear-ptr '())))
               (set-cdr! (cdr rear-ptr) element)
               (set-rp element)))))

    (define (prev element)
      (car (cdr element)))
    
    (define (rear-delete-deque!)
      (cond ((empty-deque?)
             (error "DELETE from empty deque"))
            ((null? (prev rear-ptr))
             (set-rp '())
             (set-fp '()))
            (else
             (set-rp (car (cdr rear-ptr)))
             (set-cdr! (cdr rear-ptr) '()))))

    (define (next element)
      (cdr (cdr element)))

    (define (front-delete-deque!)
      (cond ((empty-deque?)
             (error "DELETE from empty deque"))
            ((null? (next front-ptr))
             (set-rp '())
             (set-fp '()))
            (else
             (set-fp (cdr (cdr front-ptr)))
             (set-car! (cdr front-ptr) '()))))
    
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?)
             (empty-deque?))
            ((eq? m 'front-insert-deque!)
             front-insert-deque!)
            ((eq? m 'front-deque)
             front-ptr)
            ((eq? m 'rear-deque)
             rear-ptr)
            ((eq? m 'rear-insert-deque!)
             rear-insert-deque!)
            ((eq? m 'rear-delete-deque!)
             (rear-delete-deque!))
            ((eq? m 'front-delete-deque!)
             (front-delete-deque!))
            (else (error "NOT SUPPORTED --DISPATCH" m))))
    dispatch))

(define dq (make-deque))

(dq 'empty-deque?)

((dq 'front-insert-deque!) 'b)

(dq 'front-deque)

((dq 'front-insert-deque!) 'a)

(dq 'rear-deque)

((dq 'rear-insert-deque!) 'c)

((dq 'rear-insert-deque!) 'd)

(dq 'rear-delete-deque!)

(dq 'front-delete-deque!)
