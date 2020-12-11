(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

#|
Here queen-cols is recursively called for each element in enumerate
The recursive solution is called board-size time resulting in slow performance

Tree recursion in this version vs linear recursion in the older version.

If the previous solution took T time to solve the puzzle with board size 8.
this version would take roughly T^7
|#
