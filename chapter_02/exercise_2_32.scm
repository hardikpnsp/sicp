(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))

(subsets (list 1 2 3))

#| explaination

for (subsets (1 2 3))

imagine we already have subsets of (2 3) -> (() (2) (3) (2 3))

now we know that subsets of (1 2 3) -> (() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))

if we append 1 to each element of subsets of (2 3) we get ((1) (1 2) (1 3) (1 2 3))
union of these elements with subsets of (2 3) produces the subsets of (1 2 3)

we can apply this logic recursively and find the anser
|#

            
