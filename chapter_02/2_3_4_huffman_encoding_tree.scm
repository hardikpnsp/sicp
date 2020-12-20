(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67 decode the message with given tree

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;; Exercise 2.68 encode message with given tree

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol letter tree)
  (if (leaf? tree)
      '()
      (cond ((memq letter (symbols (left-branch tree)))
             (cons 0
                   (encode-symbol letter (left-branch tree))))
            ((memq letter (symbols (right-branch tree)))
             (cons 1
                   (encode-symbol letter (right-branch tree))))
            (else (error "bad symbol -- ENCODE-SYMBOL" letter)))))

(encode-symbol 'B sample-tree)

(equal? (encode (decode sample-message
                        sample-tree)
                sample-tree)
        sample-message)

;; Exercise 2.69: Generate huffman tree

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (let ((left (car leaves)))
    (if (null? (cdr leaves))
        left
        (let ((right (cadr leaves)))
          (successive-merge (adjoin-set (make-code-tree left right)
                                        (cddr leaves)))))))

(define pairs (list '(A 8) '(B 3) '(C 1) '(D 1) '(E 1) '(F 1) '(G 1) '(H 1)))

(make-leaf-set pairs)

(successive-merge (make-leaf-set pairs))

;; Exercise 2.70

(define rock-song-tree (generate-huffman-tree (list '(A 2) '(NA 16) '(BOOM 1) '(SHA 3) '(GET 2) '(YIP 9) '(JOB 2) '(WHA 1))))

(define rock-song '(get a job
                        sha na na na na na na na na
                        get a job
                        sha na na na na na na na na
                        wha yip yip yip yip yip yip yip yip yip
                        sha boom))
(encode rock-song rock-song-tree)
(decode (encode rock-song rock-song-tree) rock-song-tree)
(length (encode rock-song rock-song-tree))
;; 84 bits

;; if we use fixed lenght we will need 3 * 36 = 108 bits

;; Exercise 2.71

(generate-huffman-tree (list '(A 16) '(B 8) '(C 4) '(D 2) '(E 1)))

#| 
- 1 bit for the most frequent charecter
- n-1 bit for the least frequent

tree sketch 

            root
          /      \
most freq        node
                /    \
second most freq      node
                     /    \
      third most freq      node
                          /    \
                         .      ...

|#
