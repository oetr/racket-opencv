#! /usr/bin/env racket

;; Author: Peter Samarin
;; Load and display an image specified on the command line, then 
;; convert the image into grayscale and save it on the hard disk
;; converted from an example in opencv documentation
;; http://docs.opencv.org/doc/tutorials/tutorials.html

#lang racket/base

;;; Includes
(require opencv/highgui
         opencv/imgproc
         opencv/types
         opencv/core)


;; Get path to the image from the command line arguments
(define arguments (current-command-line-arguments))
(unless (= (vector-length arguments) 2)
  (printf "Usage: ./02-load-save-image.rkt in-image out-image~n")
  (exit))
(define image-name (vector-ref arguments 0))
(define out-image (vector-ref arguments 1))


;; Read the file
(define image (imread image-name CV_LOAD_IMAGE_COLOR))
;; Convert to gray
(define gray (cvt-color image CV_BGR2GRAY CV_8UC1))


;; Save the gray image
(imwrite out-image gray)


;; Show the two images
(imshow image-name image)
(imshow "Gray image" gray)


;; Wait for a keystroke in the window
(define key (cvWaitKey 0))
(exit 0)
