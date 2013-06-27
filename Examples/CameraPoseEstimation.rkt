#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2013
;; Description: given 4 points, rectify an image

;;; Includes
(require "../src/core.rkt"
         "../src/types.rkt"
         "../src/highgui.rkt"
         "../src/imgproc.rkt"
         "../src/calib3d.rkt"
         ffi/unsafe)

(define arguments (current-command-line-arguments))
(define image-name #f)

(if (zero? (vector-length arguments))
    (begin
      (printf "taking the default image~n")
      (set! image-name "./cube-84055-1.jpg"))
    (set! image-name (vector-ref arguments 0)))

(define source-window "Source Image")
(define rectified-window "One Rectified Marker")
(define src (imread image-name))
(define rectified-dst (cvCreateMat (CvMat-rows src)
                                   (CvMat-cols src)
                                   (CvMat-type src)))
(define edges (cvCreateMat (CvMat-rows src)
                           (CvMat-cols src)
                           CV_8UC1))
(define gray (cvCreateMat (CvMat-rows src)
                           (CvMat-cols src)
                           CV_8U))

(define H 30.0)
(define W (exact->inexact (CvMat-cols src)))

;; Manual corner detection done in the source image
(define srcPts
  (c-array _CvPoint2D32f
           (make-CvPoint2D32f 24.0 17.0)
           (make-CvPoint2D32f 80.0 30.0)
           (make-CvPoint2D32f 76.0 58.0)
           (make-CvPoint2D32f 18.0 42.0)))

;; Make the rectified marker take the whole image
(define dstPts
  (c-array _CvPoint2D32f
           (make-CvPoint2D32f 0.0 0.0)
           (make-CvPoint2D32f W   0.0)
           (make-CvPoint2D32f W   H)
           (make-CvPoint2D32f 0.0 H)))

;; compute the matrix for rectification
(define rect-mat (cvGetPerspectiveTransform (array-ptr srcPts) (array-ptr dstPts)))
(define srcPts
  (c-array _CvPoint2D32f
           (make-CvPoint2D32f 24.0 17.0)
           (make-CvPoint2D32f 80.0 30.0)
           (make-CvPoint2D32f 76.0 58.0)
           (make-CvPoint2D32f 18.0 42.0)))

(define R (cvCreateMat 3 3 CV_32FC1))
(define Rodr (cvCreateMat 3 1 CV_32FC1))
(cvZero R)
(array-ref (cvMatData-ptr R _float) 0 0)
(array-set! (cvMatData-ptr R _float) 0 0 0.45197)
(array-set! (cvMatData-ptr R _float) 0 1 0.81380)
(array-set! (cvMatData-ptr R _float) 0 2 0.36532)
(array-set! (cvMatData-ptr R _float) 1 0 -0.86341)
(array-set! (cvMatData-ptr R _float) 1 1 0.29620)
(array-set! (cvMatData-ptr R _float) 1 2 0.40839)
(array-set! (cvMatData-ptr R _float) 2 0 0.22414)
(array-set! (cvMatData-ptr R _float) 2 1 -0.5)
(array-set! (cvMatData-ptr R _float) 2 2 0.83652)
(cvRodrigues2 R Rodr)
(array-ref (cvMatData-ptr Rodr _float) 0 0)
(array-ref (cvMatData-ptr Rodr _float) 1 0)
(array-ref (cvMatData-ptr Rodr _float) 2 0)

;; 30 
(set-camera-angle 0 30 0)
(set-camera-angle 0 0 15)
(set-camera-angle 0 0 0)
(set-camera-angle 70 30 15)
(set-camera-position! 0.0 0.0 3)
(define res (glGetFloatv GL_MODELVIEW_MATRIX #:size 16))
(glGetFloatv GL_PROJECTION_MATRIX #:size 16)
(glGetFloatv GL_VIEWPORT #:size 8)


(define W 640)
(define H 480)
(define sensor-focal-length 6)
(define sensor-h 2.88)
(define tan-fov-v/2 (rad2deg (atan (/ sensor-height sensor-focal-length 2))))
(define distance-sensor-h=480 (/ (* sensor-focal-length H) 2.88))
(/ (* sensor-focal-length W) distance-sensor-h=480)
(/ (* 640 2.88) 480)
(/ (* 640 4.512) 752)

(define fov-v (compute-fov 2.88 6.0))
(define fov-h (rad2deg (* 2 (atan (/ (* W 2.88)
                                     (* H 2 6))))))


;; rectify the image
(cvWarpPerspective src rectified-dst rect-mat)
(cvCvtColor rectified-dst gray CV_BGR2GRAY)
(cvCanny gray edges 50.0 300.0 3)

;; Show the results
(imshow source-window src)
(imshow rectified-window rectified-dst)
(imshow "Gray" gray)
(imshow "Edges" edges)
;; Add a trackbar
(define low-threshold 200.0)
(define high-threshold 300.0)
(define window 3)
(define a (malloc 'atomic _int))
(ptr-set! a _int 0)
(define b (malloc 'atomic _int))
(ptr-set! b _int 0)

(define (set-threshold n (tag 'low))
  (if (symbol=? tag 'low)
      (set! low-threshold (exact->inexact n))
      (set! high-threshold (exact->inexact n)))
  (cvCanny gray edges low-threshold high-threshold window)
  (imshow "Edges" edges)
  (printf "Low: ~a, high: ~a~n" low-threshold high-threshold))

(define (on-trackbar1 n)
  (set-threshold n 'low))

(define (on-trackbar2 n)
  (set-threshold n 'high))

(cvCreateTrackbar "Low Threshold" "Edges" a 1000 on-trackbar1)
(cvCreateTrackbar "High Threshold" "Edges" b 1000 on-trackbar2)



(cvSaveImage "test.jpg" rectified-dst)

(cvWaitKey 0)
