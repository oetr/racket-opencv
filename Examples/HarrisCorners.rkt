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
      ;;(set! image-name "./images/Lena.jpg"))
    (set! image-name (vector-ref arguments 0)))

(define source-window "Source Image")
(define corners-window "Corners")
(define refined-corners-window "Refined Corners")
(define src (imread image-name))
(define src-gray  (cvCreateMat (CvMat-rows src) (CvMat-cols src) CV_8UC1))
(cvCvtColor src src-gray CV_BGR2GRAY)

(imshow source-window src)
(imshow corners-window src-gray)



(define maxCorners 10)
(define maxTrackbar 25)

(cvMat-depth src)
(cvMat-depth src-gray)


;; Parameters for Shi-Tomasi algorithm
(define corner-count (malloc 'atomic _int 1))
(ptr-set! corner-count _int 2000)
(define corners (make-c-array (ptr-ref corner-count _int) _CvPoint2D32f))
(define qualityLevel 0.001)
(define minDistance 10.0)
(define blockSize 4)
(define useHarrisDetector 1)
(define k 0.04)

;; Copy the source image
(define copy-corners (cvCloneMat src))


(cvGoodFeaturesToTrack src-gray #f #f (array-ptr corners)
                       corner-count qualityLevel minDistance #f
                       blockSize useHarrisDetector k)

;; Draw detected corners
;; (define radius 4)
;; (for ([i (ptr-ref corner-count _int)])
;;   (define corner (array-ref corners i))
;;   (cvCircle copy-corners (cvPoint (inexact->exact (round (CvPoint2D32f-x corner)))
;;                           (inexact->exact (round (CvPoint2D32f-y corner))))
;;             radius
;;             (cvScalar (random 256) (random 256) (random 256))
;;             -1 8 0))
;;(imshow corners-window copy-corners )


;; Set the neeed parameters to find the refined corners
(define winSize (make-CvSize 5 5 ))
(define zeroZone (make-CvSize  -1 -1))
(define criteria (make-CvTermCriteria (+ CV_TERMCRIT_EPS CV_TERMCRIT_ITER)
                                      40 0.001))

;; Calculate the refined corner locations
(cvFindCornerSubPix src-gray (array-ptr corners)
                    (ptr-ref corner-count _int) winSize zeroZone criteria)

(define radius 1)
;; Visualize
(for ([i (ptr-ref corner-count _int)])
  (define corner (array-ref corners i))
  (cvCircle copy-corners
            (cvPoint (inexact->exact (round (CvPoint2D32f-x corner)))
                     (inexact->exact (round (CvPoint2D32f-y corner))))
            5
            (cvScalar 255 120 20)
            0 8 0)
  (cvCircle copy-corners
            (cvPoint (inexact->exact (round (CvPoint2D32f-x corner)))
                     (inexact->exact (round (CvPoint2D32f-y corner))))
            radius
            (cvScalar 0 0 255)
            -1 8 0))

(imshow corners-window copy-corners )
(cvSaveImage "Corners.png" copy-corners)

(cvDestroyAllWindows)
