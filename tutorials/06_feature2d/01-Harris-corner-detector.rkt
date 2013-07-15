#! /usr/bin/env racket
#lang racket

;; Author: Peter Samarin
;; Date: 2013
;; Description: subpixel corner detection
;; converted from C++ code in
;; http://docs.opencv.org/doc/tutorials/features2d/trackingmotion/corner_subpixeles/corner_subpixeles.html

;;; Includes
(require (planet petr/opencv/highgui)
         (planet petr/opencv/imgproc)
         ffi/cvector)

(define arguments (current-command-line-arguments))
(define image-name #f)
(define output-image #f)

(cond [(zero? (vector-length arguments))
       (printf "taking the default image~n")
       (set! image-name "../images/Lena.jpg")
       (set! output-image "../images/Lena-Out.jpg")]
      [(= (vector-length arguments) 1)
       (set! image-name (vector-ref arguments 0))
       (set! output-image "../images/Corners.png")]
      [else
       (set! image-name (vector-ref arguments 0))
       (set! output-image (vector-ref arguments 1))])

(define src (imread image-name))
(define src-gray (cvCreateMat (CvMat-rows src) (CvMat-cols src) CV_8UC1))
(cvCvtColor src src-gray CV_BGR2GRAY)
(cvThreshold src-gray src-gray 200.0 255.0 CV_THRESH_BINARY)
(cvSmooth src-gray src-gray CV_BLUR 3 0 0.0 0.0)

;;; Previous corner detection
(define maxCorners 2000)

;; Parameters for Shi-Tomasi algorithm
(define corner-count maxCorners)
(define corners-vec (make-cvector _CvPoint2D32f corner-count))
(define qualityLevel 0.01)
(define minDistance 10.0)
(define blockSize 3)
(define useHarrisDetector 1)
(define k 0.04)

;; Copy the source image
(define copy-corners (cvCloneMat src))

(define nof-detected
      (cvGoodFeaturesToTrack src-gray #f #f corners-vec
                             corner-count qualityLevel minDistance #f
                             blockSize useHarrisDetector k))

(printf "Detected ~a corners.~n" nof-detected)


;; Set the neeed parameters to find the refined corners
(define winSize (make-CvSize 7 7))
(define zeroZone (make-CvSize  -1 -1))
(define criteria (make-CvTermCriteria (+ CV_TERMCRIT_EPS CV_TERMCRIT_ITER)
                                      3 0.001))

;; Calculate the refined corner locations
(cvFindCornerSubPix src-gray corners-vec nof-detected
                    winSize zeroZone criteria)

(define corners (cvector->list corners-vec))

(define radius 2)
;; Visualize
(for ([corner corners]
      [i (in-range 0 nof-detected)])
  (define x (CvPoint2D32f-x corner))
  (define y (CvPoint2D32f-y corner))
  (cvCircle copy-corners
            (cvPoint (inexact->exact (round x))
                     (inexact->exact (round y)))
            5 (cvScalar 255 120 20) 0 8 0)
  (cvCircle copy-corners
            (cvPoint (inexact->exact (round x))
                     (inexact->exact (round y)))
            radius (cvScalar 0 0 255) -1 8 0)
  (cvCircle src-gray
            (cvPoint (inexact->exact (round x))
                     (inexact->exact (round y)))
            5 (cvScalar 255 120 20) 0 8 0)
  (cvCircle src-gray
            (cvPoint (inexact->exact (round x))
                     (inexact->exact (round y)))
            radius (cvScalar 0 0 255) -1 8 0))

(cvSaveImage output-image copy-corners)
(imshow "gray" src-gray)
(imshow "corners" copy-corners)
(define key (cvWaitKey 0))
(cvDestroyAllWindows)
