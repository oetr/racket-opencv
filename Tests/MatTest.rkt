#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Testing CvMat structure

;;; Libraries
(require rackunit
         "../src/types.rkt"
         "../src/core.rkt"
         "../src/highgui.rkt"
         "../src/imgproc.rkt")


(define m1 (cvCreateMat 4 4 CV_32FC1))

  


