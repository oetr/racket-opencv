#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description: simple matrix manipulation

;;; libraries
(require "../src/types.rkt"
         "../src/highgui.rkt"
         "../src/core.rkt"
         "../src/imgproc.rkt"
         ffi/unsafe)

;;; a c-array of doubles
(define a (c-array _double
                   1.0 2.0 3.0 4.0
                   5.0 6.0 7.0 8.0
                   9.0 10.0 11.0 12.0))

;; make a 3x4 matrix from the c 
(define mat1 (cvMat 3 4 CV_64FC1 a))

;; empty matrix with same dimensions
(define dst (cvMat 3 4 CV_64FC1))

;; add the mat1 with itself, store the results in dst
(cvAdd mat1 mat1 dst)

;; get data array from the matrix
(define data (cvMatData-ptr dst _double))

;; access some of the elements
(array-ref data 0 0)
(array-ref data 0 1)
(array-ref data 0 2)
(array-ref data 0 3)
