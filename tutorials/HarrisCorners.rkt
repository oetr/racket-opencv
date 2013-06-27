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
       (set! image-name "images/Example2.png")
       (set! output-image "images/Corners.png")]
      [(= (vector-length arguments) 1)
       (set! image-name (vector-ref arguments 0))
       (set! output-image "images/Corners.png")]
      [else
       (set! image-name (vector-ref arguments 0))
       (set! output-image (vector-ref arguments 1))])

(define src (imread image-name))
(cvSmooth src src CV_BLUR 13 0 0.0 0.0)


(define src-gray  (cvCreateMat (CvMat-rows src) (CvMat-cols src) CV_8UC1))

(cvCvtColor src src-gray CV_BGR2GRAY)

(cvThreshold src-gray src-gray 200.0 255.0 CV_THRESH_BINARY)
(cvSmooth src-gray src-gray CV_BLUR 3 0 0.0 0.0)


;;; Previous corner detection
(define maxCorners 2000)

;; Parameters for Shi-Tomasi algorithm
(define corner-count (malloc 'atomic _int 1))
(ptr-set! corner-count _int maxCorners)
(define corners (make-c-array (ptr-ref corner-count _int) _CvPoint2D32f))
(define qualityLevel 0.0001)
(define minDistance 1.0)
(define blockSize 3)
(define useHarrisDetector 1)
(define k 0.04)

;; Copy the source image
(define copy-corners (cvCloneMat src))

(cvGoodFeaturesToTrack src-gray #f #f (array-ptr corners)
                       corner-count qualityLevel minDistance #f
                       blockSize useHarrisDetector k)

;; Set the neeed parameters to find the refined corners
(define winSize (make-CvSize 7 7))
(define zeroZone (make-CvSize  -1 -1))
(define criteria (make-CvTermCriteria (+ CV_TERMCRIT_EPS CV_TERMCRIT_ITER)
                                      3 0.001))

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
            radius (cvScalar 0 0 255) -1 8 0)
  (cvCircle src-gray
            (cvPoint (inexact->exact (round (CvPoint2D32f-x corner)))
                     (inexact->exact (round (CvPoint2D32f-y corner))))
            5 (cvScalar 255 120 20) 0 8 0)
  (cvCircle src-gray
            (cvPoint (inexact->exact (round (CvPoint2D32f-x corner)))
                     (inexact->exact (round (CvPoint2D32f-y corner))))
            radius (cvScalar 0 0 255) -1 8 0))
(cvSaveImage output-image copy-corners)
(imshow "gray" src-gray)
(imshow "corners" copy-corners)
(define key (cvWaitKey 0))
(cvDestroyAllWindows)
