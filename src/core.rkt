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

  ;; Creates an exact copy of the input matrix (except, may be, step value)
  (define-opencv-core cvCloneMat
    (_fun _pointer
          -> (mat : _pointer)
          -> (ptr-ref mat _CvMat)))

  ;; Allocates array data
  (define-opencv-core cvCreateData
    (_fun _pointer -> _void))

  #| Inline constructor. No data is allocated internally!!!
  * (Use together with cvCreateData, or use cvCreateMat instead to
  * get a matrix with allocated data):
  |#
  (define (cvMat rows cols type (data-ptr #f))
    (unless (<= (CV_MAT_DEPTH type) CV_64F)
      (raise-type-error cvMat "<= CV_64F" type))
    (define arr (cvCreateMatHeader rows cols type))
    (cvCreateData arr)
    (when data-ptr
      (ptr-set! (union-ref (CvMat-data arr) 0)
                _pointer data-ptr))
    ;;    (union-set! (CvMat-data arr) 0 (ptr-ref data-ptr _ubyte))
    arr)
  

  ;; Releases array data
  (define-opencv-core cvReleaseData
    (_fun _pointer -> _void))

  #| Attaches user data to the array header. The step is reffered to
  the pre-last dimension. That is, all the planes of the array
  must be joint (w/o gaps) |#
  (define-opencv-core cvSetData
    (_fun _pointer _pointer _int -> _void))

  #| Retrieves raw data of CvMat, IplImage or CvMatND.
  In the latter case the function raises an error if
  the array can not be represented as a matrix |#
  (define-opencv-core cvGetRawData
    (_fun (arr data (step #f) (roi-size #f)) ::
          (arr : _pointer)
          (data : (_ptr io (_ptr io _ubyte)))
          (step : _pointer)
          (roi-size : _pointer)
          -> _void))

  ;; Returns width and height of array in elements
  (define-opencv-core cvGetSize
    (_fun _pointer -> _void))

  ;; Clears all the array elements (sets them to 0)
  (define-opencv-core cvSetZero
    (_fun _pointer -> _void))

  ;; Copies source array to destination array
  (define-opencv-core cvCopy (_fun (src : _pointer)
                                   (dst : (_ptr i _IplImage))
                                   _pointer
                                   -> _void))
  
  (define-opencv-core cvAddS (_fun _pointer _CvScalar _pointer _pointer -> _void))

  (define (make-c-array size type)
    (ptr-ref (malloc type 'atomic)
             (_array type size)))

  (define (c-array type . vals)
    (define an-array (make-c-array (length vals) type))
    (for ([val (in-list vals)]
          [i (in-range 0 (length vals))])
         (array-set! an-array i val))
    an-array)

  ;; Drawing
  (define (CV_RGB r g b)
    (cvScalar b g r 0))
  (define CV_FILLED -1)
  (define CV_AA 16)

  #| basic font types |#
  (define CV_FONT_HERSHEY_SIMPLEX         0)
  (define CV_FONT_HERSHEY_PLAIN           1)
  (define CV_FONT_HERSHEY_DUPLEX          2)
  (define CV_FONT_HERSHEY_COMPLEX         3)
  (define CV_FONT_HERSHEY_TRIPLEX         4)
  (define CV_FONT_HERSHEY_COMPLEX_SMALL   5)
  (define CV_FONT_HERSHEY_SCRIPT_SIMPLEX  6)
  (define CV_FONT_HERSHEY_SCRIPT_COMPLEX  7)

  ;; font flags
  (define CV_FONT_ITALIC                 16)

  (define CV_FONT_VECTOR0    CV_FONT_HERSHEY_SIMPLEX)
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


  ;; Initializes font structure used further in cvPutText
  (define-opencv-core cvInitFont
    (_fun (font-face hscale vscale (shear 0.0) (thickness 1) (line-type 8)) ::
          (font : _pointer = (malloc _CvFont 'atomic))
          (font-face : _int)
          (hscale : _double)
          (vscale : _double)
          (shear : _double)
          (thickness : _int)
          (line-type : _int)
          -> (font : _pointer)
          -> (ptr-ref font _CvFont)))

  
  (define (cvFont scale (thickness 1))
    (cvInitFont CV_FONT_HERSHEY_PLAIN scale scale 0.0 thickness CV_AA))

  #| Draws a rectangle given two opposite corners of the rectangle (pt1 & pt2),
  if thickness<0 (e.g. thickness == CV_FILLED), the filled box is drawn |#
  (define (cvRectangle img pt1 pt2 color (thickness 1) (line_type 8) (shift 0))
    (define-opencv-core cvRectangle
      (_fun _pointer _CvPoint _CvPoint _CvScalar _int
            _int _int -> _void))
    (cvRectangle img pt1 pt2 color thickness line_type shift))


  #| Renders text stroke with specified font and color at specified location.
  CvFont should be initialized with cvInitFont |#
  (define-opencv-core cvPutText
    (_fun _pointer _string _CvPoint _pointer _CvScalar
          -> _void))
)