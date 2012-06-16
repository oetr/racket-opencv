#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; converted from an example in opencv documentation:
;;http://docs.opencv.org/doc/tutorials/imgproc/gausian_median_blur_bilateral_filter/gausian_median_blur_bilateral_filter.html
;; this example loads an image
;; Applies 4 different kinds of filters and shows the filtered images sequentially

;;; Includes
(require "../src/types.rkt"
         "../src/highgui.rkt"
         "../src/core.rkt"
         "../src/imgproc.rkt"
         ffi/unsafe
         ffi/unsafe/define)

(define (display-caption caption)  
  (cvSetZero dst)
  (define font1 (malloc _CvFont 'atomic))
  (cvInitFont font1 CV_FONT_HERSHEY_SIMPLEX 0.5 0.5)
  (cvPutText dst caption
             (make-CvPoint (/ (CvMat-cols src) 4)
                           (/ (CvMat-rows src) 2))
             font1
             (CV_RGB 120 120 120))
  (imshow window-name dst)
  (if (>= (cvWaitKey DELAY_CAPTION) 0) -1 0))

(define (display-dst delay)
  (imshow window-name dst)
  (if (>= (cvWaitKey delay) 0) -1 0))

;; Global Variables
(define DELAY_CAPTION     1500)
(define DELAY_BLUR        100)
(define MAX_KERNEL_LENGTH 31)

(define window-name "Filter Demo 1")

;; load an image into a Mat array
(define src (imread "images/Lena.jpg"))
(define dst (cvMat (CvMat-rows src) (CvMat-cols src) (CvMat-type src)))

(when (not (zero? (display-caption "Original Image")))
  (exit))

(set! dst (cvCloneMat src))

(when (not (zero? (display-dst DELAY_CAPTION)))
  (exit))

;;; Applying Homogeneous blur
(when (not (zero? (display-caption "Homogeneous Blur")))
  (exit))

(for ([i (in-range 1 MAX_KERNEL_LENGTH 2)])
     (cvSmooth src dst CV_BLUR i 0 0.0 0.0)
     (when (not (zero? (display-dst DELAY_BLUR)))
       (exit)))

;;; Applying Gaussian blur
(when (not (zero? (display-caption "Gaussian Blur")))
  (exit))

(for ([i (in-range 1 MAX_KERNEL_LENGTH 2)])
     (cvSmooth src dst CV_GAUSSIAN i 0 0.0 0.0)
     (when (not (zero? (display-dst DELAY_BLUR)))
       (exit)))

;;; Applying Median blur
(when (not (zero? (display-caption "Median Blur")))
  (exit))

(for ([i (in-range 1 MAX_KERNEL_LENGTH 2)])
     (cvSmooth src dst CV_MEDIAN i 0 0.0 0.0)
     (when (not (zero? (display-dst DELAY_BLUR)))
       (exit)))

;;; Applying Bilateral blur
(when (not (zero? (display-caption "Bilateral Blur")))
  (exit))

(for ([i (in-range 1 MAX_KERNEL_LENGTH 2)])
     (cvSmooth src dst CV_BILATERAL i 0 (* i 2.0) (/ i 2.0))
     (when (not (zero? (display-dst DELAY_BLUR)))
       (exit)))

;;; Wait until user press a key
(display-caption "End: Press a key!")
(cvWaitKey 0)
(exit)
