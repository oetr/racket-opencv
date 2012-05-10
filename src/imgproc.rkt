;; Author: Petr Samarin
;; Description: Porting imgproc_c.h to Racket

(module core racket
  (provide (all-defined-out))
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define)

  (define-ffi-definer define-opencv-imgproc
    (ffi-lib "/opt/local/lib/libopencv_imgproc"))

  (require "types.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Image processing procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  ;; Erodes input image (applies minimum filter) one or more times.
  ;; If element pointer is NULL, 3x3 rectangular element is used.
  (define-opencv-imgproc cvErode
    (_fun _pointer _pointer _pointer _int -> _void))

  ;; Dilates input image (applies maximum filter) one or more times.
  ;; If element pointer is NULL, 3x3 rectangular element is used.
  (define-opencv-imgproc cvDilate
    (_fun _pointer _pointer _pointer _int -> _void))

  ;; Calculates an image derivative using generalized Sobel
  ;; (aperture_size = 1,3,5,7) or Scharr (aperture_size = -1) operator.
  ;; Scharr can be used only for the first dx or dy derivative */
  (define-opencv-imgproc cvSobel
    (_fun _pointer _pointer _int _int _int -> _void))

    ;; Runs canny edge detector
  (define-opencv-imgproc cvCanny
    (_fun _pointer _pointer _double _double _int -> _void))

  ;; Converts input array pixels from one color space to another
  (define-opencv-imgproc cvCvtColor
    (_fun _pointer _pointer _int -> _void))

  #| Calculates constraint image for corner detection
   Dx^2 * Dyy + Dxx * Dy^2 - 2 * Dx * Dy * Dxy.
  Applying threshold to the result gives coordinates of corners |#
  (define-opencv-imgproc cvPreCornerDetect
    (_fun _pointer _pointer _int -> _void))

  ;; Harris corner detector:
  ;; Calculates det(M) - k*(trace(M)^2),
  ;; where M is 2x2 gradient covariation matrix for each pixel
  (define-opencv-imgproc cvCornerHarris
    (_fun _pointer _pointer _int _int _double -> _void))
)