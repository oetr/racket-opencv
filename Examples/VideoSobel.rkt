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

;; Open video capture of camera 0
(define capture (cvCaptureFromCAM 0))

;; Init variable in which captured images will be stored
(define captured-image (cvQueryFrame capture))

;; Get parameters from the captured image to initialize
;; copied images
(define size (make-CvSize (IplImage-width captured-image)
                          (IplImage-height captured-image)))
(define depth (IplImage-depth captured-image))
(define channels (IplImage-nChannels captured-image))

;; Init an IplImage to where captured images will be copied
(define frame (cvCreateImage size depth channels))

;; Init an IplImage to store results of sobel edge detection
;; it must have a higher bit width than 8U of the original image
;; because of potential overflow
(define sobel-frame (cvCreateImage size IPL_DEPTH_16S channels))

(let loop ()  
  ;; Store captured frame in IplImage format (do not edit this frame!)
  (set! captured-image (cvQueryFrame capture))

  ;; Copy the IplImage into another one,
  ;; so that we can display and modify it
  (cvCopy captured-image frame #f)

  ;; Sobel edge detection
  (cvSobel frame sobel-frame 2 0 7)
  
  ;; Show the frame on the screen
  (cvShowImage "Video Capture" sobel-frame)
  
  ;; Wait for 1 ms for a key  
  ;; Break out from the loop if any key has been pressed
  ;; key = -1 if no key has been pressed
  (unless (>= (cvWaitKey 1) 0)
    (loop)))

;; Stop capture
(cvReleaseCapture capture)

;; Destroy image window
(cvDestroyWindow "Video Capture")
