;; Author: Petr Samarin
;; Description: Porting highgui_c.h to Racket

(module core racket
  (provide (all-defined-out))
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define)

  (define-ffi-definer define-opencv-core
    (ffi-lib "/opt/local/lib/libopencv_core"))

  (require "types.rkt")

;;; FFI Definers
  (define-opencv-core cvSetImageROI (_fun _pointer _CvRect -> _void))

  (define-opencv-core cvAddS (_fun _pointer _CvScalar _pointer _pointer -> _void))

  (define-opencv-core cvCopy (_fun (src : _pointer)
                                   (dst : (_ptr i _IplImage))
                                   _pointer
                                   -> _void))

  (define-opencv-core cvCreateImage (_fun _CvSize _int _int
                                          -> (ipl-image : (_ptr io _IplImage))
                                          -> (ptr-ref ipl-image _IplImage)))


  (define (make-c-array size type)
    (define a (_array type size))
    (define ptr (malloc type 'atomic))
    (ptr-ref ptr a))

  ;; (define (array-filter fn array min max)
  ;;   (if (= min max) empty
  ;;       (let ([value (array-ref array min)])        
  ;;         (if (fn value)
  ;;             (cons value (array-filter fn array (+ min 1) max))
  ;;             (array-filter fn array (+ min 1) max)))))

  ;; (define a (make-c-array 20 _int))
  ;; (array-filter (lambda (x) (< x 10)) a 20)

;;; Structs
  ;; (define data (_union (_cpointer _ubyte)))
  ;; (define-cstruct _CvMat
  ;;   ([type _int]
  ;;    [step _int]
  ;;    ;; for internal use only
  ;;    [refcount _gcpointer]
  ;;    [hdr_refcount _int]
  ;;    ))
;;; Procedures
  ;;(define-opencv-highgui cvCreateMat (_fun _int _int _int -> _pointer))

)