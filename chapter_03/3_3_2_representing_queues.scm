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
