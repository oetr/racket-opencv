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
         ffi/unsafe)

(define arguments (current-command-line-arguments))
(define image-name #f)

(if (zero? (vector-length arguments))
    (begin
      (printf "taking the default image~n")
      (set! image-name "./images/ArucoRectification.png"))
    (set! image-name (vector-ref arguments 0)))

(define source-window "Source Image")
(define rectified-window "One Rectified Marker")
(define src (imread image-name))
(define rectified-dst (cvCreateMat (CvMat-rows src)
                                   (CvMat-cols src)
                                   (CvMat-type src)))

;; Manual corner detection done in the source image
(define srcPts
  (c-array _CvPoint2D32f
           (make-CvPoint2D32f 195.0 134.0)
           (make-CvPoint2D32f 404.0 120.0)
           (make-CvPoint2D32f 424.0 317.0)
           (make-CvPoint2D32f 167.0 326.0)))

;; Make the rectified marker take the whole image
(define dstPts
  (c-array _CvPoint2D32f
           (make-CvPoint2D32f 0.0 0.0)
           (make-CvPoint2D32f 640.0 0.0)
           (make-CvPoint2D32f 640.0 480.0)
           (make-CvPoint2D32f 0.0 480.0)))

;; compute the matrix for rectification
(define rect-mat (cvGetPerspectiveTransform (array-ptr srcPts) (array-ptr dstPts)))

;; rectify the image
(cvWarpPerspective src rectified-dst rect-mat)

;; Show the results
(imshow source-window src)
(imshow rectified-window rectified-dst)

(cvWaitKey 0)
