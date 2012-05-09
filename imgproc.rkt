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

)