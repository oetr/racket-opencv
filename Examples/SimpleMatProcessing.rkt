#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; inspired by the opencv documentation:
;;http://docs.opencv.org/doc/tutorials/imgproc/gausian_median_blur_bilateral_filter/gausian_median_blur_bilateral_filter.html
;; this example loads an image
;; Applies 4 different kinds of filters and shows the filtered images sequentially

;;; Includes
(require "../src/types.rkt"
         "../src/highgui.rkt"
         "../src/core.rkt"
         "../src/imgproc.rkt"
         ffi/unsafe
         ffi/unsafe/define)

(define a (c-array _ubyte 1 2 3 4 5 6 7 8 9 10 11 12))

(define type CV_64FC1)

(define m1 (cvCreateMatHeader 3 4 type))

(cvCreateData m1)

(cvSetData m1 (array-ptr a) CV_AUTOSTEP)

(define x (cvMatData-ptr m1 0))

(for ([i (in-bytes x)]) (printf "~a~n" i))

(cvSetZero m1)

(define img1 (make-CvMat
              (bitwise-ior CV_MAT_MAGIC_VAL CV_MAT_CONT_FLAG type) ;; type
              (* 4 (CV_ELEM_SIZE type)) ;; step
              #f ;; refcount
              0 ;; hdr_refcount
              (ptr-ref (array-ptr a) CvMatUnion-data)
              4
              4))

(make-sized-byte-string (union-ref (CvMat-data img1) 4)
                        (* (CvMat-rows img1)
                           (CvMat-cols img1)))

(cvMat 10 10 8 #f)
(define (display-caption caption)
  ())

 int display_caption( char* caption )
 {
   dst = Mat::zeros( src.size(), src.type() );
   putText( dst, caption,
            Point( src.cols/4, src.rows/2),
            CV_FONT_HERSHEY_COMPLEX, 1, Scalar(255, 255, 255) );

   imshow( window_name, dst );
   int c = waitKey( DELAY_CAPTION );
   if( c >= 0 ) { return -1; }
   return 0;
  }

;; Global Variables
(define DELAY_CAPTION     1500)
(define DELAY_BLUR        100)
(define MAX_KERNEL_LENGTH 31)

(define window-name "Filter Demo 1")


;; load an image into a Mat array
(define src (imread "test1.png"))


