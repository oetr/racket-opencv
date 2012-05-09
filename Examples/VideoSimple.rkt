#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; Capture video from a camera
;; if some key is pressed while the focus is on the video window, the application terminates
;; Tested with iSight camera of my MacBook Pro

;;; Includes
(require "../src/types.rkt")
(require "../src/highgui.rkt")
(require "../src/core.rkt")
(require "../src/imgproc.rkt")

(define capture (cvCaptureFromCAM 0))
(define captured-image (cvQueryFrame capture))

;; Get parameters from the captured image to initialize
;; copied images
(define width (IplImage-width captured-image))
(define height (IplImage-height captured-image))
(define size (make-CvSize width height))
(define depth (IplImage-depth captured-image))
(define channels (IplImage-nChannels captured-image))

;; Init an IplImage to where captured images will be copied
(define frame (cvCreateImage size depth channels))

(let loop ()  
  (set! captured-image (cvQueryFrame capture))
  (cvCopy captured-image frame #f)
  (cvShowImage "Video Capture" frame)  
  (unless (>= (cvWaitKey 1) 0)
    (loop)))

(cvReleaseCapture capture)
(cvDestroyWindow "Video Capture")