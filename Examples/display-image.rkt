#! /usr/bin/env racket
#lang racket

;; Author: Peter Samarin
;; Description: Load and display an image specified on the command line
;; converted from an example in opencv documentation
;; http://docs.opencv.org/doc/tutorials/tutorials.html

;;; Includes
(require "../src/core.rkt"
         "../src/highgui.rkt")

(define arguments (current-command-line-arguments))
(unless (= (vector-length arguments) 1)
  (printf "Usage: ./display-image.rkt imageToLoadAndDisplay~n")
  (exit))

;; Read the file
(define image (imread (vector-ref arguments 0) CV_LOAD_IMAGE_COLOR))
(imshow "Display window" image)


(define key (cvWaitKey 0)) ;; Wait for a keystroke in the window
(exit 0)
