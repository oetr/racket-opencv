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
(require "../src/core.rkt")
(require "../src/highgui.rkt")
(require "../src/imgproc.rkt")


(define capture (cvCaptureFromCAM 0))

;; Reduce image resolution to 640x480
(define success? (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_WIDTH 640.0))
(set! success? (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_HEIGHT 480.0))

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
(define frame (cvCreateImage size depth channels))
(printf "depth = ~a~n" depth)
(define grey-frame (cvCreateImage size IPL_DEPTH_8U 1))

(define corners (cvCreateImage size IPL_DEPTH_32F 1))

(let loop ()  
  (set! captured-image (cvQueryFrame capture))
  (cvCvtColor captured-image grey-frame CV_BGR2GRAY)
  ;;(cvCopy captured-image frame #f)
  ;; (cvErode grey-frame grey-frame #f 1)
  ;; (cvDilate grey-frame grey-frame #f 1)
  ;; (cvErode grey-frame grey-frame #f 1)
  ;; (cvDilate grey-frame grey-frame #f 1)
  ;; (cvCanny grey-frame grey-frame 30.0 30.0 3)
  ;; (cvCanny grey-frame grey-frame 100.0 100.0 3)
  ;;(cvErode grey-frame grey-frame #f 10)
  ;;(cvSmooth grey-frame grey-frame CV_GAUSSIAN 5 5 0.0 0.0)
  (cvCornerHarris grey-frame corners 5 7 0.001)
  (cvShowImage "Video Capture" corners)
  (unless (>= (cvWaitKey 1) 0)
    (loop)))

(cvReleaseCapture capture)
(cvDestroyWindow "Video Capture")