#! /usr/bin/env racket
#lang racket

;; Author: Peter Samarin
;; Description: Load and display an image specified on the command line
;; converted from an example in opencv documentation
;; http://docs.opencv.org/doc/tutorials/tutorials.html

;;; Includes
(require "../../src/core.rkt"
         "../../src/types.rkt"
         "../../src/highgui.rkt"
         "../../src/imgproc.rkt")

;; Get path to the image from the command line arguments
(define arguments (current-command-line-arguments))
(unless (= (vector-length arguments) 1)
  (printf "Usage: ./display-image.rkt imageToLoadAndDisplay~n")
  (exit))
(define image-name (vector-ref arguments 0))


;; Read the file
(define image (imread "../images/Lena.jpg" CV_LOAD_IMAGE_COLOR))
;; Convert to gray
(define gray (cvt-color image CV_BGR2GRAY CV_8UC1))


;; Save the gray image
(imwrite "../images/gray-image.jpg" gray)


;; Show the two images
(imshow image-name image)
(imshow "Gray image" gray)


;; Wait for a keystroke in the window
(define key (cvWaitKey 0))
(exit 0)
