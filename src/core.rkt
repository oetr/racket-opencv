;; Author: Petr Samarin
;; Description: Porting highgui_c.h to Racket

(module core racket
  (provide (all-defined-out))
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/vector)

  (define-ffi-definer define-opencv-core
    (ffi-lib "/opt/local/lib/libopencv_core"))

  (require "types.rkt")

;;; FFI Definers
  #|***************************************************************************************\
*          Array allocation, deallocation, initialization and access to elements         *
  \***************************************************************************************|#

  #| Allocates and initializes IplImage header |#
  (define-opencv-core cvCreateImageHeader (_fun _CvSize _int _int
                                          -> (ipl-image : (_ptr io _IplImage))
                                          -> (ptr-ref ipl-image _IplImage)))

  #| Inializes IplImage header |#
  (define-opencv-core cvInitImageHeader (_fun _pointer _CvSize _int _int _int _int
                                          -> (ipl-image : (_ptr io _IplImage))
                                          -> (ptr-ref ipl-image _IplImage)))

  #| Creates IPL image (header and data) |#
  ;; (define-opencv-core cvCreateImage (_fun _CvSize _int _int
  ;;                                         -> (ipl-image : (_ptr i _IplImage))
  ;;                                         -> (ptr-ref ipl-image _IplImage)))
  
   ;; (define-opencv-core cvCreateImage
   ;;   (_fun _CvSize _int _int ->
   ;;         -> (img : _pointer)
   ;;         -> (ptr-ref img _IplImage)))

  ;; (define-opencv-core cvCreateImage
  ;;   (_cprocedure (list _CvSize _int _int) _IplImage
  ;;                (lambda ()
   (define-opencv-core cvCreateImage
     (_cprocedure (list _CvSize _int _int) _pointer
                  #:atomic? #t
                  #:wrapper
                  (lambda (ffi-obj)
                    (lambda (size depth channels)
                      (ptr-ref (ffi-obj size depth channels) _IplImage)))))

           ;; -> (img : _pointer = (malloc 'atomic _IplImage))
           ;; -> (ptr-ref img _IplImage)))
   
  ;; (define (create-image size depth channels)
  ;;   (define img (malloc 'atomic _IplImage))
  ;;   (ptr-ref(cvCreateImage size depth channels)
    
    
  ;;   ()
  ;;                                         -> (ipl-image : (_ptr i _IplImage))
  ;;                                         -> (ptr-ref ipl-image _IplImage)))

  #| Releases (i.e. deallocates) IPL image header |#
  (define-opencv-core cvReleaseImageHeader
    (_fun (_ptr i _pointer) -> _void))

  #| Releases IPL image header and data |#
  (define-opencv-core cvReleaseImage
    (_fun (_ptr i _pointer) -> _void))

  (define (cvReleaseImages . images)
    (andmap cvReleaseImage images))
  
  #| Creates a copy of IPL image (widthStep may differ) |#
  (define-opencv-core cvCloneImage
    (_fun _pointer
          -> (ipl-image : (_ptr io _IplImage))
          -> (ptr-ref ipl-image _IplImage))) 

  #| Sets a Channel Of Interest (only a few functions support COI) -
  use cvCopy to extract the selected channel and/or put it back |#
  (define-opencv-core cvSetImageCOI
    (_fun _pointer _int -> _void))

  #| Retrieves image Channel Of Interest |#
  (define-opencv-core cvGetImageCOI
    (_fun _pointer -> _int))

  #| Sets image ROI (region of interest) (COI is not changed) |#
  (define-opencv-core cvSetImageROI
    (_fun _pointer _CvRect -> _void))

  #| Resets image ROI and COI |#
  (define-opencv-core cvResetImageROI
    (_fun _IplImage -> _void))

  #| Retrieves image ROI |#
  (define-opencv-core cvGetImageROI
    (_fun _pointer -> _CvRect))

  #| Allocates and initalizes CvMat header |#
  (define-opencv-core cvCreateMatHeader
    (_fun _int _int _int
          -> (mat : (_ptr io _CvMat))
          -> (ptr-ref mat _CvMat)))
  
  (define CV_AUTOSTEP  #x7fffffff)


  #| Initializes CvMat header |#
  (define-opencv-core cvInitMatHeader
    (_fun _pointer _int _int _int _pointer _int
          -> (mat : (_ptr io _CvMat))
          -> (ptr-ref mat _CvMat)))

  #| Allocates and initializes CvMat header and allocates data |#
  (define-opencv-core cvCreateMat
    (_fun _int _int _int
          -> (mat : (_ptr io _CvMat))
          -> (ptr-ref mat _CvMat)))

  #| Releases CvMat header and deallocates matrix data
  (reference counting is used for data) |#
  (define-opencv-core cvReleaseMat
    (_fun (_ptr i _pointer) -> _void))

  (define-opencv-core cvAddS (_fun _pointer _CvScalar _pointer _pointer -> _void))

  (define-opencv-core cvCopy (_fun (src : _pointer)
                                   (dst : (_ptr i _IplImage))
                                   _pointer
                                   -> _void))


  (define (make-c-array size type)
    (define a (_array type size))
    (define ptr (malloc type 'atomic))
    (ptr-ref ptr a))


  ;; Font structure
  (define-cstruct _CvFont
    ([nameFont _string]     ;; Qt:nameFont
     ;; Qt:ColorFont -> cvScalar(blue_component, green_component,
     ;; red\_component[, alpha_component])
     [color _CvScalar]
     [font_face _int]       ;; Qt: bool italic         =CV_FONT_
     [ascii (_ptr i _int)]  ;; font data and metrics
     [greek _string]
     [cyrillic (_ptr i _int)]
     [hscale _float]
     [vscale _float]
     [shear  _float]        ;; slope coefficient: 0 - normal, >0 - italic
     [thickness  _int]      ;; Qt: weight   letters thickness
     [dx     _float]        ;; horizontal interval between letters
     [line_type _int]))	    ;; Qt: PointSize

  ;; (define (array-filter fn array min max)
  ;;   (if (= min max) empty
  ;;       (let ([value (array-ref array min)])        
  ;;         (if (fn value)
  ;;             (cons value (array-filter fn array (+ min 1) max))
  ;;             (array-filter fn array (+ min 1) max)))))

  ;; (define a (make-c-array 20 _int))
  ;; (array-filter (lambda (x) (< x 10)) a 20)


  ;; Drawing
  (define CV_FILLED -1)
  #| Draws a rectangle given two opposite corners of the rectangle (pt1 & pt2),
  if thickness<0 (e.g. thickness == CV_FILLED), the filled box is drawn |#
  (define (cvRectangle img pt1 pt2 color (thickness 1) (line_type 8) (shift 0))
    (define-opencv-core cvRectangle
      (_fun _pointer _CvPoint _CvPoint _CvScalar _int
            _int _int -> _void))
     (cvRectangle img pt1 pt2 color thickness line_type shift))

)