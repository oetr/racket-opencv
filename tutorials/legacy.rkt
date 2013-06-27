(define (draw-contours! lof-sequences img thickness)
  (define (random+ limit addition)
    (+ (random limit) addition))
  (andmap (lambda (a-sequence)
            (define color (CV_RGB (random+ 155 100)
                                  (random+ 155 100)
                                  (random+ 155 100)))
            (cvDrawContours img a-sequence  color color -1 thickness)
            (cvClearSeq a-sequence))
          lof-sequences))

(define (sequence-chain->list a-seq)
  (define next-ptr (CvSeq-h_next a-seq))  
  (if next-ptr
      (let ([next-sequence (ptr-ref next-ptr _CvSeq)])
        (cons a-seq (sequence-chain->list next-sequence)))
      (cons a-seq empty)))


(define (get-coordinates a-seq)
  (for/vector ([i (CvSeq-total a-seq)])
    (define point (ptr-ref (cvGetSeqElem a-seq i) _CvPoint))
    (list (CvPoint-x point) (CvPoint-y point))))

(define make-point list)
(define pt-x car)
(define pt-y cadr)

(define (pt- pt1 pt2)
  (make-point (- (pt-x pt1) (pt-x pt2))
              (- (pt-y pt1) (pt-y pt2))))

(define (pt+ pt1 pt2)
  (make-point (+ (pt-x pt1) (pt-x pt2))
              (+ (pt-y pt1) (pt-y pt2))))

(define (pt*s pt scalar)
  (make-point (* (pt-x pt) scalar)
              (* (pt-y pt) scalar)))

;; to compute the slope of 2 points
(define (slope pt1 pt2)
  (define diff-y (abs (- (pt-y pt1) (pt-y pt2))))
  (define diff-x (abs (- (pt-x pt1) (pt-x pt2))))
  (if (zero? diff-x)
      10
      (/ diff-y diff-x 1.0)))

(define (dot-product v1 v2)
  (apply + (map * v1 v2)))

(define (cross-product v1 v2)
  (- (* (pt-x v1) (pt-y v2))
     (* (pt-y v1) (pt-x v2))))

;; find the andle between two vectors defined by their
;; intersection point p2
(define (find-angle p1 p2 p3)
  (define v1 (pt- p1 p2))
  (define v2 (pt- p3 p2))
  (define d-mul (* (ed v1) (ed v2)))
  (define angle
    (if (< (abs d-mul) 1e-7)
        pi
        (acos (/ (dot-product v1 v2)
                 d-mul 1.0))))
  (real-part angle))

(define (ed pt)
  (sqrt (+ (sqr (pt-x pt)) (sqr (pt-y pt)))))

(define (line-intersection o1 p1 o2 p2)
  (define x (pt- o2 o1))
  (define d1 (pt- p1 o1))
  (define d2 (pt- p2 o2))
  (define cross (cross-product d1 d2))
  (define cross2 (cross-product x d2))
  (if (< cross 1e-8)
      #f
      (let* ([t1 (/ cross2 cross 1.0)]
             [intersection (pt+ o1 (pt*s d1 t1))])
        ;; round the intersection
        (map inexact->exact (map round intersection)))))

(define (remove-close-points seq (min-distance 5))
  (define len (vector-length seq))
  (define previous-point (vector-ref seq 0))
  (for/fold ([result (vector previous-point)]) ([i (in-range 1 len)])
    (define point (vector-ref seq i))
    (define distance (ed (pt- previous-point point)))
    (if (> distance min-distance)
        (begin 
          (set! previous-point point)
          (vector-append (vector point) result))
        result)))

(define (reduce-contour seq (threshold 1))
  (define len (vector-length seq))
  (define lines
    (let loop ([i 0] [result (list )])
      (if (= i len)
          result
          (let ([next (modulo (+ i 1) len)])
            (define next2 (modulo (+ i 2) len))
            (define p1 (vector-ref seq i))
            (define p2 (vector-ref seq next))
            (define p3 (vector-ref seq next2))
            (define angle (find-angle p1 p2 p3))
            (define center (cvPoint (pt-x p2) (pt-y p2)))
            (if (or (<= (abs angle) (degrees->radians threshold))
                    (>= (abs angle) (degrees->radians (- 180.0 threshold))))
                (loop (+ i 1) (cons (list p1 p3) result))
                (loop (+ i 1) result))))))
  lines)

(define W 640)
(define H 480)

(define (find-and-draw-angles img seq (threshold 2))
  (define len (vector-length seq))
  (define font1 (malloc _CvFont 'atomic))
  (cvInitFont font1 CV_FONT_HERSHEY_SIMPLEX 0.4 0.4)
  (define lines (reduce-contour seq threshold))
  ;; compute the direciton of each line
  (for/list ([line (in-list lines)])
    (define p1 (car line))
    (define p2 (cadr line))
    (define v (pt- p2 p1))
    ;; find line intersections with the image borders
    (define new-points
      (filter identity
              (list (line-intersection p1 p2 (list 0 0) (list 0 H))
                    (line-intersection p1 p2 (list 0 0) (list W 0))
                    (line-intersection p1 p2 (list W H) (list 0 H))
                    (line-intersection p1 p2 (list W H) (list W 0)))))
    ;; find the angle of the line
    (define hypothenuse (sqrt (+ (sqr (pt-x v)) (sqr (pt-y v)))))
    (if (= (length new-points) 2)
        (begin
          (cons (car new-points)
                (cons (cadr new-points)
                      (cons p1 (cons p2 (list (radians->degrees
                                               (asin (/ (pt-y v) hypothenuse)))))))))
        (begin
          (cons p1 (cons p2 (list (radians->degrees
                                   (asin (/ (pt-y v) hypothenuse))))))))))

(define (draw-lines img lines)
  (andmap (lambda (a-line)
            (define pt1 (car a-line))
            (define pt2 (cadr a-line))
            (cvLine img (cvPoint (pt-x pt1) (pt-y pt1))
                    (cvPoint (pt-x pt2) (pt-y pt2))
                    (cvScalar (random 255) (random 255) (random 255)) 1 8 0))
          lines))


(define (detect-and-draw-lines sequences)
  (andmap (lambda (seq)
            (draw-lines src-clone
                        (find-and-draw-angles src-clone
                                              (remove-close-points
                                               (get-coordinates
                                                seq)
                                               10)
                                              0.1)))
          sequences))

(define src-clone (cvCloneMat src))
(detect-and-draw-lines sequences)
(imshow "clone" src-clone)
(cvSaveImage "test.png" src-clone)



(define (detect-and-draw-lines-1 a-seq)
  (draw-lines src-clone
              (find-and-draw-angles src-clone
                                    (remove-close-points
                                     (get-coordinates
                                      a-seq)
                                     10)
                                    1)))

(define src-clone (cvCloneMat src))
(andmap detect-and-draw-lines-1 sequences)
(imshow "clone" src-clone)
(cvSaveImage "test.png" src-clone)

(cvReleaseMemStorage storage)
