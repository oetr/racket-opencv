
;; Author: Petr Samarin
;; Description: Porting highgui_c.h to Racket

(module core racket
  (provide (all-defined-out))
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/vector
           "types.rkt"
           "utilities.rkt")

  (define-ffi-definer define-opencv-core
    (ffi-lib "/opt/local/lib/libopencv_core"))

  #|***********************************************************************
  *  Array allocation, deallocation, initialization and access to elements 
  \************************************************************************|#
  
  #| Allocates and initializes IplImage header |#
  (define-opencv-core cvCreateImageHeader (_fun _CvSize _int _int
                                                -> (ipl-image : (_ptr io _IplImage))
                                                -> (ptr-ref ipl-image _IplImage)))

  #| Inializes IplImage header |#
  (define-opencv-core cvInitImageHeader (_fun _pointer _CvSize _int _int _int _int
                                              -> (ipl-image : (_ptr io _IplImage))
                                              -> (ptr-ref ipl-image _IplImage)))

  #| Creates IPL image (header and data) |#
  (define-opencv-core cvCreateImage
    (_fun _CvSize _int _int
          -> (img : _pointer)
          -> (ptr-ref img _IplImage)))

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
    (_fun _pointer
          -> (r : _int)
          -> (check-return r 'cvGetImageCOI)))

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
      ;; find out the right type and point to 0--4
      ;; for now use ptr = 0
      (define ptr 0)
      (union-set! (CvMat-data arr) ptr (array-ptr data-ptr)))
    arr)
  

  ;; Releases array data
  (define-opencv-core cvReleaseData
    (_fun (_ptr i _pointer) -> _void))

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
    (_fun _pointer -> _CvSize))

  ;; Clears all the array elements (sets them to 0)
  (define-opencv-core cvSetZero
    (_fun _pointer -> _void))

  (define cvZero cvSetZero)

  ;; Copies source array to destination array
  (define-opencv-core cvCopy
    (_fun (src dst (mask #f)) ::
          (src : _pointer)
          (dst : _pointer)
          (mask : _pointer)
          -> _void))

  (define (copy-image img (mask #f))
    (cond [(IplImage? img)
           (define out (cvCreateImage (make-CvSize (IplImage-width img)
                                                   (IplImage-height img))
                                      (IplImage-depth img)
                                      (IplImage-nChannels img)))
           (cvCopy img out mask)
           out]
          [(CvMat? img)
           (define out (cvMat (IplImage-width img)
                              (IplImage-height img)
                              (IplImage-depth img)
                              (IplImage-nChannels img)))
           (cvCopy img out mask)
           out]))

  ;; dst(mask) = src1(mask) + src2(mask)
  (define-opencv-core cvAdd
    (_fun (src1 src2 dst (mask #f)) ::
          (src1 : _pointer)
          (src2 : _pointer)
          (dst : _pointer)
          (mask  : _pointer)
          -> _void))

  (define-opencv-core cvAddS (_fun _pointer _CvScalar _pointer _pointer -> _void))

  (define (make-c-array size type)
    (ptr-ref (malloc 'atomic type size)
             (_array type size)))

  (define (c-array type . vals)
    (define an-array (make-c-array (length vals) type))
    (for ([val (in-list vals)]
          [i (in-range 0 (length vals))])
         (array-set! an-array i val))
    an-array)


  #|**************************************************************************
  Dynamic data structures                                  
  ****************************************************************************|#

  ;; Calculates length of sequence slice (with support of negative indices).
  (define-opencv-core cvSliceLength
    (_fun _CvSlice _pointer
          -> (r : _int)
          -> (check-return r 'cvSliceLength)))


  #| Creates new memory storage.
  block_size == 0 means that default,
  somewhat optimal size, is used (currently, it is 64K) |#
  (define-opencv-core cvCreateMemStorage
    (_fun ((block-size 0)) :: (block-size : _int) -> (storage : _pointer)
          -> (ptr-ref storage _CvMemStorage)))

  ;; Creates a memory storage that will borrow memory blocks from parent storage
  (define-opencv-core cvCreateChildMemStorage
    (_fun _pointer -> _pointer))

  #|Releases memory storage. All the children of a parent must be released before
  the parent. A child storage returns all the blocks to parent when it is released |#
  (define-opencv-core cvReleaseMemStorage
    (_fun (_ptr i _pointer) -> _void))

  #| Clears memory storage. This is the only way(!!!) (besides cvRestoreMemStoragePos)
  to reuse memory allocated for the storage - cvClearSeq,cvClearSet ...
  do not free any memory.
  A child storage returns all the blocks to the parent when it is cleared |#
  (define-opencv-core cvClearMemStorage
    (_fun _pointer -> _void))

  #| Remember a storage "free memory" position |#
  (define-opencv-core cvSaveMemStoragePos
    (_fun _pointer _pointer -> _void))

  #| Restore a storage "free memory" position |#
  (define-opencv-core cvRestoreMemStoragePos
    (_fun _pointer _pointer -> _void))

  ;; Allocates continuous buffer of the specified size in the storage */
  (define-opencv-core cvMemStorageAlloc
    (_fun _pointer _int -> _pointer))

  ;; ;; Allocates string in memory storage */
  ;; (define-opencv-core cvMemStorageAllocString
  ;;   (_fun (storage ptr (len -1)) ::
  ;;         [storage : _pointer]
  ;;         [ptr : _pointer]
  ;;         [len : _int]
  ;;         -> _CvString))

  ;; Creates new empty sequence that will reside in the specified storage */
  (define-opencv-core cvCreateSeq
    (_fun _int _int _int _pointer
          -> (seq : _pointer)
          -> (ptr-ref seq _CvSeq)))

  #| Changes default size (granularity) of sequence blocks.
  The default size is ~1Kbyte |#
  (define-opencv-core cvSetSeqBlockSize
    (_fun _pointer _int -> _void))


  ;; Adds new element to the end of sequence. Returns pointer to the element */
  (define-opencv-core cvSeqPush
    (_fun (seq (element #f)) ::
          [seq : _pointer]
          [element : _pointer]
          -> _pointer))

  ;; Adds new element to the beginning of sequence. Returns pointer to it */
  (define-opencv-core cvSeqPushFront
    (_fun (seq (element #f)) ::
          [seq : _pointer]
          [element : _pointer]
          -> _pointer))

  ;; Removes the last element from sequence and optionally saves it */
  (define-opencv-core cvSeqPop
    (_fun (seq (element #f)) ::
          [seq : _pointer]
          [element : _pointer]
          -> _void))


  ;; Removes the first element from sequence and optioanally saves it */
  (define-opencv-core cvSeqPopFront
    (_fun (seq (element #f)) ::
          [seq : _pointer]
          [element : _pointer]
          -> _void))

  (define CV_FRONT 1)
  (define CV_BACK 0)


  #| Removes all the elements from the sequence. The freed memory
  can be reused later only by the same sequence unless cvClearMemStorage
  or cvRestoreMemStoragePos is called |#
  (define-opencv-core cvClearSeq
    (_fun _pointer -> _void))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Drawing
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (_fun (font-ptr font-face hscale vscale (shear 0.0) (thickness 1) (line-type 8)) ::
          (font-ptr : _pointer)
          (font-face : _int)
          (hscale : _double)
          (vscale : _double)
          (shear : _double)
          (thickness : _int)
          (line-type : _int)
          -> _void))

  
  (define (cvFont scale (thickness 1))
    (cvInitFont CV_FONT_HERSHEY_PLAIN scale scale 0.0 thickness CV_AA))

  #| Draws a rectangle given two opposite corners of the rectangle (pt1 & pt2),
  if thickness<0 (e.g. thickness == CV_FILLED), the filled box is drawn |#
  (define-opencv-core cvRectangle
    (_fun (img pt1 pt2 color (thickness 1) (line-type 8) (shift 0)) ::
          (img : _pointer)
          (pt1 : _CvPoint)
          (pt2 : _CvPoint)
          (color : _CvScalar)
          (thickness : _int)
          (line-type : _int)
          (shift : _int) -> _void))

  #| Draws 4-connected, 8-connected or antialiased line segment connecting two points|#
  (define-opencv-core cvLine
    (_fun (img pt1 pt2 color (thickness 1) (line-type 8) (shift 0)) ::
          (img : _pointer)
          (pt1 : _CvPoint)
          (pt2 : _CvPoint)
          (color : _CvScalar)
          (thickness : _int)
          (line-type : _int)
          (shift : _int) -> _void))

  #| Renders text stroke with specified font and color at specified location.
  CvFont should be initialized with cvInitFont |#
  (define-opencv-core cvPutText
    (_fun _pointer _string _CvPoint _pointer _CvScalar
          -> _void))

  #| Draws contour outlines or filled interiors on the image |#
  (define-opencv-core cvDrawContours
    (_fun (img contour external-color hole-color max-level
               (thickness 1) (line-type 8) (offset (cvPoint 0 0))) ::
               [img : _pointer]
               [contour : _pointer]
               [external-color : _CvScalar]
               [hole-color : _CvScalar]
               [max-level : _int]
               [thickness : _int]
               [line-type : _int]
               [offset : _CvPoint] -> _void))  

  #|************************ Adding own types **************************|#
  (define-opencv-core cvRegisterType
    (_fun _pointer -> _void))

  (define-opencv-core cvUnregisterType
    (_fun _string -> _void))
  
  #| universal functions |#
  (define-opencv-core cvRelease
    (_fun _pointer -> _void))

  (define-opencv-core cvClone
    (_fun _pointer -> _pointer))

  #| simple API for reading/writing data |#
  (define-opencv-core cvSave
    (_fun (filename struct-ptr (name #f) (comment #f) (attributes (cvAttrList))) ::
          [filename : _file]
          [struct-ptr : _pointer]
          [name : _string]
          [comment : _string]
          [attributes : _CvAttrList]
          -> _void))

  (define-opencv-core cvLoad
    (_fun (filename (memstorage #f) (name #f) (real_name #f)) ::
          [filename : _file]
          [memstorage : _pointer]
          [name : _string]
          [real_name : _string]
          -> _pointer))

  )
