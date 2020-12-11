;;a
(define (outline-painter frame)
  (let ((p1 (origin frame))
        (p2 (add-vec p1 (edge1 frame)))
        (p3 (add-vec p1 (edge2 frame)))
        (p4 (add-vec p2 (edge2 frame))))
    (segment->painter (list (make-segment p1 p2)
                            (make-segment p1 p3)
                            (make-segment p2 p4)
                            (make-segment p3 p4)))))
;;b
(define (x-painter frame)
  (let ((p1 (origin frame))
        (p2 (add-vec p1 (edge1 frame)))
        (p3 (add-vec p1 (edge2 frame)))
        (p4 (add-vec p2 (edge2 frame))))
    (segment->painter (list (make-segment p1 p4)
                            (make-segmetn p2 p3)))))

;;c
(define (diamond-painter frame)
  (define (mid-point-vec vec1 vec2)
    (scale-vec 0.5 (add-vec vec1 vec2)))
  (let ((p1 (origin frame))
        (p2 (add-vec p1 (edge1 frame)))
        (p4 (add-vec p1 (edge2 frame)))
        (p3 (add-vec p2 (edge2 frame))))
    (let ((mp1 (mid-point-vec p1 p2))
          (mp2 (mid-point-vec p2 p3))
          (mp3 (mid-point-vec p3 p4))
          (mp4 (mid-point-vec p4 p1)))
      (segment->painter (list (make-segment mp1 mp2)
                              (make-segment mp2 mp3)
                              (make-segment mp3 mp4)
                              (make-segment mp4 mp1))))))

;;d
#|
small segments from big segments
|#
