#! /usr/bin/env racket
#lang racket
;; Author: Peter Samarin
;; Description: Load, display, and save an image specified on the command line
;; converted from an example in opencv documentation
;; http://docs.opencv.org/doc/tutorials/tutorials.html

(require (planet petr/opencv))


(define arguments (current-command-line-arguments))
(unless (= (vector-length arguments) 1)
  (printf "No image data~n")
  (exit))


(define image (imread (vector-ref arguments 0) CV_LOAD_IMAGE_COLOR))
(imshow "Display window" image)


(define key (cvWaitKey 0))


(exit 0)