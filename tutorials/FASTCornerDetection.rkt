#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; FAST corner detection that computes corner score for every pixel in the image
;; First, this algorithm was implemented in Racket, but it was too slow
;; the C implementation is 5x-10x faster
;; TODO: run a window to find the pixel with the maximum corner score given a neighborhood size
;; This implementation is inspired by: "HIGH-SPEED IMAGE FEATURE DETECTION USING FPGA
;; IMPLEMENTATION OF FAST ALGORITHM" by Kraft, Schmidt, Kasinski, (2008).
;; available from: http://nguyendangbinh.org/Proceedings/VISIGRAPP/2008/VISIGRAPP%202008/VISAPP%202008%20Volume%201/Posters/C1_224_Kraft.pdf

(require ffi/unsafe
         ffi/unsafe/define)

;;; Includes
(require "../src/types.rkt"
         "../src/core.rkt"
         "../src/highgui.rkt"
         "../src/imgproc.rkt"
         "../src/iplhelper.rkt")

;;; Constants
(define THRESHOLD 20)
(define CIRCLE-LENGTH 16)

(define capture (cvCaptureFromCAM 0))

;; Capture an image to get parameters
(define captured-image (cvQueryFrame capture))
(cvNamedWindow "Video Capture" CV_WINDOW_AUTOSIZE)

;; Add a trackbar
(define a (malloc 'atomic _int))
(ptr-set! a _int THRESHOLD)
(define (on-trackbar n)
  (set! THRESHOLD n)
  (printf "Threshold: ~a~n" THRESHOLD))
(cvCreateTrackbar "Corner Threshold" "Video Capture" a 50 on-trackbar)


;; Containers for copying and processing camera frames
(define frame (cvCreateImage
               (make-CvSize (/ (IplImage-width captured-image) 2)
                            (/ (IplImage-height captured-image) 2))
               IPL_DEPTH_8U 1))

(define corners (cvCreateImage
                 (make-CvSize (/ (IplImage-width captured-image) 2)
                              (/ (IplImage-height captured-image) 2))
                 (IplImage-depth captured-image)
                 (IplImage-nChannels captured-image)))

(let loop ()
  (set! captured-image (cvQueryFrame capture))
  (cvPyrDown captured-image corners)
  (cvCvtColor corners frame CV_BGR2GRAY)
  (detect_corners frame corners THRESHOLD)
  (cvShowImage "Video Capture" corners)
  (unless (>= (cvWaitKey 1) 0)
    (loop)))

;; clean up
(cvReleaseImages frame corners)
(cvReleaseCapture capture)
