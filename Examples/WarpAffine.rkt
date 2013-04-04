#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; converted from an example in opencv documentation:
;; http://docs.opencv.org/doc/tutorials/imgproc/imgtrans/warp_affine/warp_affine.html

;;; Includes
(require "../src/core.rkt"
         "../src/types.rkt"
         "../src/highgui.rkt"
         "../src/imgproc.rkt"
         ffi/unsafe)

(define arguments (current-command-line-arguments))
(define image-name #f)

(if (zero? (vector-length arguments))
  (begin
    (printf "provide image name~n")
    (exit))
  (set! image-name (vector-ref arguments 0)))

;;(define image-name "images/Lena.jpg")

(define source-window "Source Image")
(define warp-window "Warp")
(define warp-rotate-window "Warp + Rotate")

(define rot-mat (cvCreateMat 2 3 CV_32FC1))
(define warp-mat (cvCreateMat 2 3 CV_32FC1))

(define src (imread image-name))
(define warp-dst (cvCreateMat (CvMat-rows src) (CvMat-cols src) (CvMat-type src)))
(define warp-rotate-dst (cvCreateMat (CvMat-rows src)
                                     (CvMat-cols src)
                                     (CvMat-type src)))

;; (define srcTri
;;   (c-array _CvPoint2D32f
;;            (make-CvPoint2D32f 0.0                      0.0)
;;            (make-CvPoint2D32f (- (CvMat-cols src) 1.0) 0.0)
;;            (make-CvPoint2D32f 0.0                      (- (CvMat-rows src) 1.0))))

;; (define dstTri
;;   (c-array _CvPoint2D32f
;;            (make-CvPoint2D32f (* (CvMat-cols src) 0.0)  (* (CvMat-rows src) 0.33))
;;            (make-CvPoint2D32f (* (CvMat-cols src) 0.85) (* (CvMat-rows src) 0.25))
;;            (make-CvPoint2D32f (* (CvMat-cols src) 0.15) (* (CvMat-rows src) 0.7))))

(define srcTri (malloc 'atomic _CvPoint2D32f 3))
(ptr-set! srcTri _CvPoint2D32f 0 (make-CvPoint2D32f 0.0                      0.0))
(ptr-set! srcTri _CvPoint2D32f 1 (make-CvPoint2D32f (- (CvMat-cols src) 1.0) 0.0))
(ptr-set! srcTri _CvPoint2D32f 2 (make-CvPoint2D32f 0.0 (- (CvMat-rows src) 1.0)))

(define dstTri (malloc 'atomic _CvPoint2D32f 3))
(ptr-set! dstTri _CvPoint2D32f 0
          (make-CvPoint2D32f (* (CvMat-cols src) 0.0)  (* (CvMat-rows src) 0.33)))
(ptr-set! dstTri _CvPoint2D32f 1
          (make-CvPoint2D32f (* (CvMat-cols src) 0.85) (* (CvMat-rows src) 0.25)))
(ptr-set! dstTri _CvPoint2D32f 2
          (make-CvPoint2D32f (* (CvMat-cols src) 0.15) (* (CvMat-rows src) 0.7)))

;; Get the Affine Transform
(cvGetAffineTransform srcTri dstTri warp-mat)

;; Apply the Affine Transform just found to the src image
(cvWarpAffine src warp-dst warp-mat)

;; Rotating the image after Warp

;; Compute a rotation matrix with respect to the center of the image
(define center (make-CvPoint2D32f (/ (CvMat-cols warp-dst) 2.0)
                                  (/ (CvMat-rows warp-dst) 2.0)))
(define angle -50.0)
(define scale 0.6)

;; Get the rotation matrix with the specifications above
(define rot-mat (cv2DRotationMatrix center angle scale))

;; Rotate the warped image
(cvWarpAffine warp-dst warp-rotate-dst rot-mat)

;; Show what you got
(cvNamedWindow source-window CV_WINDOW_AUTOSIZE)
(imshow source-window src)

(cvNamedWindow warp-window CV_WINDOW_AUTOSIZE)
(imshow warp-window warp-dst)

(cvNamedWindow warp-rotate-window CV_WINDOW_AUTOSIZE)
(imshow warp-rotate-window warp-rotate-dst)
