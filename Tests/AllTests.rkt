#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Testing everything

;;; Libraries
(require rackunit
         "../src/types.rkt"
         "../src/core.rkt"
         "../src/highgui.rkt"
         "../src/imgproc.rkt")

;;; Loading, showing and closing
(define frame (cvCreateImage size depth channels))
(define grey-frame (cvCreateImage size IPL_DEPTH_8U 1))
(define corners (cvCreateImage size IPL_DEPTH_32F 1))

(cvShowImage "Video Capture" corners)
(cvWaitKey 1)

;;; Creating empty images


;;; Image manipulation
(cvCvtColor captured-image grey-frame CV_BGR2GRAY)
(cvCopy captured-image frame #f)
(cvErode grey-frame grey-frame #f 1)
(cvDilate grey-frame grey-frame #f 1)
(cvErode grey-frame grey-frame #f 1)
(cvDilate grey-frame grey-frame #f 1)
(cvCanny grey-frame grey-frame 30.0 30.0 3)
(cvCanny grey-frame grey-frame 100.0 100.0 3)
(cvErode grey-frame grey-frame #f 10)
(cvSmooth grey-frame grey-frame CV_GAUSSIAN 5 5 0.0 0.0)
(cvCornerHarris grey-frame corners 5 7 0.001)


;;; Testing the camera
(define capture (cvCaptureFromCAM 0))
(define captured-image (cvQueryFrame capture))
(cvReleaseCapture capture)
(cvDestroyWindow "Video Capture")

;; Get parameters from the captured image to initialize
;; copied images
(define width    (IplImage-width captured-image))
(define height   (IplImage-height captured-image))
(define size     (make-CvSize width height))
(define depth    (IplImage-depth captured-image))
(define channels (IplImage-nChannels captured-image))


;;; Testing camera properties
(cvSetCaptureProperty capture CV_CAP_PROP_FRAME_WIDTH 640.0)
(cvSetCaptureProperty capture CV_CAP_PROP_FRAME_HEIGHT 480.0)


;;; Capturing images from the camera
(set! captured-image (cvQueryFrame capture))

;;; Image processing
