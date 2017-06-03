#! /usr/bin/env racket
#lang racket

;; Author: Peter Samarin
;; Date: 2013
;; Description: Sobel detection on camera images
;; Tested with iSight camera of my MacBook Pro

;;; Includes
(require opencv/core
         opencv/types
         opencv/highgui
         opencv/imgproc
         opencv/videoio)

;; Open video capture of camera 0
(define capture (cvCaptureFromCAM 0))

;; Init variable in which captured images will be stored
(define captured-image (cvQueryFrame capture))

;; Get parameters from the captured image to initialize
;; copied images
(define size (make-CvSize (IplImage-width captured-image)
                          (IplImage-height captured-image)))
(define half-size (make-CvSize (/ (IplImage-width captured-image) 2)
                               (/ (IplImage-height captured-image) 2)))
(define depth (IplImage-depth captured-image))
(define channels (IplImage-nChannels captured-image))

;; Init an IplImage to where captured images will be copied
(define big-frame (cvCreateImage size depth channels))
(define frame (cvCreateImage size depth channels))
(define frame-gray (cvCreateImage size IPL_DEPTH_8U 1))

;; Init an IplImage to store results of sobel edge detection
;; it must have a higher bit width than 8U of the original image
;; because of potential overflow
(define sobel-frame-x (cvCreateImage size IPL_DEPTH_16S 1))
(define sobel-frame-y (cvCreateImage size IPL_DEPTH_16S 1))
(define abs-grad-x (cvCreateImage size IPL_DEPTH_8U 1))
(define abs-grad-y (cvCreateImage size IPL_DEPTH_8U 1))
(define grad (cvCreateImage size IPL_DEPTH_8U 1))

(let loop ()  
  ;; Store captured frame in IplImage format (do not edit this frame!)
  (set! captured-image (cvQueryFrame capture))

  ;; Copy the IplImage into another one,
  ;; so that we can display and modify it
  (cvCopy captured-image big-frame #f)
  (cvCvtColor big-frame frame-gray CV_RGB2GRAY)

  ;; Sobel edge detection
  (cvSobel frame-gray sobel-frame-x 1 0 3)
  (cvConvertScaleAbs sobel-frame-x abs-grad-x)
  
  (cvSobel frame-gray sobel-frame-y 0 1 3)
  (cvConvertScaleAbs sobel-frame-y abs-grad-y)

  (cvAddWeighted abs-grad-x 0.5 abs-grad-y 0.5 0.0 grad )
  (cvSmooth grad grad CV_GAUSSIAN 3 0 0.0 0.0)

  ;; Show the frame on the screen
  (cvShowImage "Weighted" grad)
  
  ;; Wait for 1 ms for a key  
  ;; Break out from the loop if any key has been pressed
  ;; key = -1 if no key has been pressed
  (unless (>= (cvWaitKey 1) 0)
    (loop)))

;; Stop capture
(cvReleaseCapture capture)

;; Destroy image window
(cvDestroyAllWindows)
