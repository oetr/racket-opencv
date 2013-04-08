#! /usr/bin/env racket
#lang racket

;; Author: Peter Samarin
;; Date: 2013
;; Finds fiducial markers in an image

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

;; take a default image if no other image was provided
(cond [(zero? (vector-length arguments))
       (printf "taking the default image~n")
       (set! image-name "images/Example4.png")
       (set! output-image "images/Corners.png")]
      [(= (vector-length arguments) 1)
       (set! image-name (vector-ref arguments 0))
       (set! output-image "images/Corners.png")]
      [else
       (set! image-name (vector-ref arguments 0))
       (set! output-image (vector-ref arguments 1))])

(define src (imread image-name))

;; simulate blurred images
(cvSmooth src src CV_BLUR 1 0 0.0 0.0)

;; make a grey image
(define src-gray  (cvCreateMat (CvMat-rows src) (CvMat-cols src) CV_8UC1))
(cvCvtColor src src-gray CV_BGR2GRAY)

;; threshold the gray image
(cvThreshold src-gray src-gray 30.0 255.0 CV_THRESH_BINARY)
(imshow "gray" src-gray)

;; detect contours
(define storage (cvCreateMemStorage 0))
(define contour (malloc 'atomic _pointer))
(cvFindContours src-gray storage (ptr-ref contour _CvSeq)
                128
                CV_RETR_EXTERNAL;; CV_CHAIN_APPROX_TC89_KCOS);;CV_CHAIN_APPROX_TC89_L1)
;;CV_RETR_EXTERNAL
;;CV_CHAIN_APPROX_TC89_L1)
;;CV_CHAIN_APPROX_SIMPLE)
;;CV_RETR_TREE;; CV_RETR_TREE
CV_CHAIN_APPROX_NONE)

(define (sequence-chain->list a-seq)
  (define next-ptr (CvSeq-h_next a-seq))  
  (if next-ptr
      (let ([next-sequence (ptr-ref next-ptr _CvSeq)])
        (cons a-seq (sequence-chain->list next-sequence)))
      (cons a-seq empty)))


(define seq (ptr-ref (ptr-ref contour _pointer) _CvSeq))
(define sequences (sequence-chain->list seq))
;;(map CvSeq-total sequences)

;; for each sequence, find bounding rectangle
(define rects (map cvMinAreaRect2 sequences))
(define r1 (list-ref rects 12))
;; (cvRectangle src-clone (make-CvPoint (CvRect-x r1) (CvRect-y r1))
;;              (make-CvPoint (+ (CvRect-x r1) (CvRect-width r1))
;;                            (+ (CvRect-y r1) (CvRect-height r1)))
;;              (CV_RGB 255 0 0))

(define bounding-rect (cvBoxPoints r1))
(define pt1 (car bounding-rect))
(define pt2 (cadr bounding-rect))
(define pt3 (caddr bounding-rect))
(define pt4 (cadddr bounding-rect))
(set! pt1 (make-CvPoint (inexact->exact (round (CvPoint2D32f-x pt1)))
                        (inexact->exact (round (CvPoint2D32f-y pt1)))))
(set! pt2 (make-CvPoint (inexact->exact (round (CvPoint2D32f-x pt2)))
                        (inexact->exact (round (CvPoint2D32f-y pt2)))))
(set! pt3 (make-CvPoint (inexact->exact (round (CvPoint2D32f-x pt3)))
                        (inexact->exact (round (CvPoint2D32f-y pt3)))))
(set! pt4 (make-CvPoint (inexact->exact (round (CvPoint2D32f-x pt4)))
                        (inexact->exact (round (CvPoint2D32f-y pt4)))))
(define src-clone (cvCloneMat src))                                

(cvLine src-clone pt1 pt2 (CV_RGB 255 0 0))
(cvLine src-clone pt2 pt3 (CV_RGB 255 0 0))
(cvLine src-clone pt3 pt4 (CV_RGB 255 0 0))
(cvLine src-clone pt4 pt1 (CV_RGB 255 0 0))

(imshow "gray" src-clone)
(CvSeq-total (car sequences))

(define m (cvCreateMat 196 1 CV_32FC2))
(cvZero m)
(define m1 (cvCreateMat 196 1 CV_32FC1))
(cvZero m1)
(define m2 (cvCreateMat 196 1 CV_32FC1))
(cvZero m2)
(define m3 (cvCreateMat 196 1 CV_32FC1))
(cvZero m3)
(define m4 (cvCreateMat 196 1 CV_32FC1))
(cvZero m4)

(cvCvtSeqToArray (car sequences)
                 (cvGetRawData m))
(cvSplit m m1 m2 #f #f)

(define min-i (malloc 'atomic _double))
(define max-i (malloc 'atomic _double))
(define min-loc (malloc 'atomic _CvPoint))
(define max-loc (malloc 'atomic _CvPoint))
(cvMinMaxLoc m1 min-i max-i min-loc max-loc)
(ptr-ref min-i _int 0)
(ptr-ref max-i _int 0)
(CvPoint-y (ptr-ref min-loc _CvPoint))
(CvPoint-x (ptr-ref max-loc _CvPoint))


(define p (cvGetRawData m1))
(for/fold ([min-val 640] [max-i 0] [max-val 0] [min-i 0]) ([i (in-range 0 101)])
  (define x (ptr-ref p _int i))
  (define new-min min-val)
  (define new-max max-val)
  (if (> min-i x)
      (set! min-val ))
  (values (min min-i x)
          i
          (max max-i x)
          i))

;; (for ([a-sequence sequences])
;;   (define color (CV_RGB 255 0 0))
;;   (cvDrawContours src-clone a-sequence color color 0 2))

(imshow "gray" src-clone)
