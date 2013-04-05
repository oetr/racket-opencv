#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2013
;; Description: subpixel corner detection
;; converted from C++ code in
;; http://docs.opencv.org/doc/tutorials/features2d/trackingmotion/corner_subpixeles/corner_subpixeles.html

;;; Includes
(require "../src/core.rkt"
         "../src/types.rkt"
         "../src/highgui.rkt"
         "../src/imgproc.rkt"
         racket/format
         ffi/unsafe)

(define arguments (current-command-line-arguments))
(define image-name #f)
(define output-image #f)

(cond [(zero? (vector-length arguments))
       (printf "taking the default image~n")
       (set! image-name "images/ArucoRectification.png")
       (set! output-image "images/Corners.png")]
      [(= (vector-length arguments) 1)
       (set! image-name (vector-ref arguments 0))
       (set! output-image "images/Corners.png")]
      [else
       (set! image-name (vector-ref arguments 0))
       (set! output-image (vector-ref arguments 1))])

(define src (imread image-name))
;;(cvSmooth src src CV_BLUR 31 0 0.0 0.0)


(define src-gray  (cvCreateMat (CvMat-rows src) (CvMat-cols src) CV_8UC1))

(cvCvtColor src src-gray CV_BGR2GRAY)

(cvThreshold src-gray src-gray 50.0 255.0 CV_THRESH_BINARY)
(imshow "gray" src-gray)

(define storage (cvCreateMemStorage 0))
(define contour (malloc 'atomic _pointer))
(cvFindContours src-gray storage (ptr-ref contour _CvSeq)
                128
                CV_RETR_EXTERNAL;; CV_CHAIN_APPROX_TC89_KCOS);;CV_CHAIN_APPROX_TC89_L1)
;;CV_RETR_EXTERNAL
;;CV_CHAIN_APPROX_TC89_L1
;;CV_CHAIN_APPROX_SIMPLE)
;;CV_RETR_TREE;; CV_RETR_TREE
CV_CHAIN_APPROX_NONE)

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

(define src-clone (cvCloneMat src))
;; (when (ptr-ref contour _pointer)
;;   (define seq (ptr-ref (ptr-ref contour _pointer) _CvSeq))
;;   (define sequences (sequence-chain->list seq))
;;   (draw-contours! sequences src-clone 1))


;; (cvDrawContours src-clone (car sequences)
;;                 (CV_RGB 0 100 255)
;;                 (CV_RGB 0 100 255) -1 1)

(define seq (ptr-ref (ptr-ref contour _pointer) _CvSeq))
(define sequences (sequence-chain->list seq))
(map CvSeq-total sequences)

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
  (if (< (abs d-mul) 1e-7)
      pi
      (acos (/ (dot-product v1 v2)
               d-mul 1.0))))

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
            (if (or (<= angle (degrees->radians threshold))
                    (>= angle (degrees->radians (- 180.0 threshold))))
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
        (cons (car new-points)
              (cons (cadr new-points)
                    (cons p1 (cons p2 (list (radians->degrees
                                             (asin (/ (pt-y v) hypothenuse))))))))
        (begin
          (cons p1 (cons p2 (list (radians->degrees
                                   (asin (/ (pt-y v) hypothenuse))))))))))

(define (draw-lines img lines)
  (andmap (lambda (a-line)
            (define pt1 (car a-line))
            (define pt2 (cadr a-line))
            (cvLine img (cvPoint (pt-x pt1) (pt-y pt1))
                    (cvPoint (pt-x pt2) (pt-y pt2))
                    (cvScalar (random 255) (random 255) (random 255)) 2 8 0))
          lines))

(define src-clone (cvCloneMat src))
(draw-lines src-clone
            (find-and-draw-angles src-clone
                                  (remove-close-points
                                   (get-coordinates
                                    (list-ref sequences 4))
                                   20)
                                 0.1))
(imshow "clone" src-clone)
(cvSaveImage "test.png" src-clone)



(define maxCorners 2000)
(define maxTrackbar 25)

;; Parameters for Shi-Tomasi algorithm
(define corner-count (malloc 'atomic _int 1))
(ptr-set! corner-count _int maxCorners)
(define corners (make-c-array (ptr-ref corner-count _int) _CvPoint2D32f))
(define qualityLevel 0.0001)
(define minDistance 5.0)
(define blockSize 2)
(define useHarrisDetector 1)
(define k 0.04)

;; Copy the source image
(define copy-corners (cvCloneMat src))

(cvGoodFeaturesToTrack src-gray #f #f (array-ptr corners)
                       corner-count qualityLevel minDistance #f
                       blockSize useHarrisDetector k)

;; Set the neeed parameters to find the refined corners
(define winSize (make-CvSize 5 5 ))
(define zeroZone (make-CvSize  -1 -1))
(define criteria (make-CvTermCriteria (+ CV_TERMCRIT_EPS CV_TERMCRIT_ITER)
                                      1 0.01))

;; Calculate the refined corner locations
(cvFindCornerSubPix src-gray (array-ptr corners) (ptr-ref corner-count _int)
                    winSize zeroZone criteria)

(define radius 1)
;; Visualize
(for ([i (ptr-ref corner-count _int)])
  (define corner (array-ref corners i))
  (cvCircle copy-corners
            (cvPoint (inexact->exact (round (CvPoint2D32f-x corner)))
                     (inexact->exact (round (CvPoint2D32f-y corner))))
            5 (cvScalar 255 120 20) 0 8 0)
  (cvCircle copy-corners
            (cvPoint (inexact->exact (round (CvPoint2D32f-x corner)))
                     (inexact->exact (round (CvPoint2D32f-y corner))))
            radius (cvScalar 0 0 255) -1 8 0))
(cvSaveImage output-image copy-corners)
(cvDestroyAllWindows)

