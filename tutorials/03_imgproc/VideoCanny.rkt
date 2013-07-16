#! /usr/bin/env racket
#lang racket

;; Author: Peter Samarin
;; Date: 2013
;; Description: Capture video from a camera, apply canny filter
;; if some key is pressed while the focus is on the video window,
;; then the application terminates
;; Tested with iSight camera of my MacBook Pro

;;; Includes
(require (planet petr/opencv/highgui)
         (planet petr/opencv/imgproc))

(define capture (cvCaptureFromCAM 0))

;; Capture an image to get parameters
(define captured-image (cvQueryFrame capture))

;; Get parameters from the captured image to initialize
;; copied images
(define width    (IplImage-width captured-image))
(define height   (IplImage-height captured-image))
(define size     (make-CvSize width height))
(define depth    (IplImage-depth captured-image))
(define channels (IplImage-nChannels captured-image))

;; Init an IplImage to where captured images will be copied
(define frame (cvCreateImage size depth 1))
(define out #f)

(let loop ()
  (set! captured-image (cvQueryFrame capture))
  (cvConvertImage captured-image frame IPL_DEPTH_8U)
  (cvSmooth frame frame CV_GAUSSIAN 11 11 0.0 0.0)
  (cvCanny frame frame 50.0 100.0 3)
  (cvShowImage "Video Canny" frame)
  (unless (>= (cvWaitKey 1) 0)
    (loop)))

;; clean up (not really necessary, because the application exits)
(cvReleaseCapture capture)
(cvDestroyAllWindows)
