;;; Libraries
;; Unit Testing
(require rackunit)
;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define)
;; OpenCV Constants
(require "opencv-constants.rkt")
(require "types.rkt")

;;; FFI Definers
(define-ffi-definer define-opencv-highgui
  (ffi-lib "/opt/local/lib/libopencv_highgui"))

(define-ffi-definer define-opencv-core
  (ffi-lib "/opt/local/lib/libopencv_core"))

;;; Structures
(define-cstruct _CvPoint
  ([x _int]
   [y _int]))

;; Constants
(define CV_LOAD_IMAGE_UNCHANGED  -1) ;; 8bit color or not
(define CV_LOAD_IMAGE_GRAYSCALE  0)  ;; 8bit gray
(define CV_LOAD_IMAGE_COLOR      1)  ;; ? color
(define CV_LOAD_IMAGE_ANYDEPTH   2)  ;; any depth ?
(define CV_LOAD_IMAGE_ANYCOLOR   4)  ;; ? any color

;;; Procedures
(define-opencv-highgui cvLoadImage (_fun _string _int -> _pointer))
(define-opencv-highgui cvNamedWindow (_fun _string _int -> _int))
(define-opencv-highgui cvShowImage (_fun _string _pointer -> _void))
(define-opencv-highgui cvDestroyWindow (_fun _string -> _void))

;;; Tests
(define img
  (ptr-ref
   (cvLoadImage "test.png" CV_LOAD_IMAGE_COLOR) _IplImage))

(cvNamedWindow "Main Window" CV_WINDOW_AUTOSIZE)
(cvShowImage "Main Window" img)
(cvDestroyWindow "Main Window")