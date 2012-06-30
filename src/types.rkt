(module types racket
  (provide (all-defined-out))

  (require ffi/unsafe
           ffi/unsafe/define)  

  #| CvArr* is used to pass arbitrary
  * array-like data structures
  * into functions where the particular
  * array type is recognized at runtime:
  |#
  (define CvArr _void)
  
  (define-cstruct _Cv32suf ([i _int]
                            [u _uint]
                            [f _float]))

  (define-cstruct _Cv64suf ([i _int64]
                            [u _uint64]
                            [f _double]))
  
  (define CVStatus _int)
  (define CV_StsOk   0 );; everything is ok
  (define CV_StsBackTrace  -1);;   pseudo error for back trace  
  (define CV_StsError  -2);;   unknown /unspecified error
  (define CV_StsInternal  -3);;   internal error (bad state)
  (define CV_StsNoMem  -4);;   insufficient memory 
  (define CV_StsBadArg  -5);;   function arg/param is bad 
  (define CV_StsBadFunc  -6);;   unsupported function
  (define CV_StsNoConv  -7);;   iter. didn't converge  
  (define CV_StsAutoTrace  -8);;   tracing 
  (define CV_HeaderIsNull  -9);;   image header is NULL
  (define CV_BadImageSize  -10);;  image size is invalid  
  (define CV_BadOffset  -11);;  offset is invalid
  (define CV_BadDataPtr  -12);; 
  (define CV_BadStep  -13);; 
  (define CV_BadModelOrChSeq  -14);; 
  (define CV_BadNumChannels  -15);; 
  (define CV_BadNumChannel1U  -16);; 
  (define CV_BadDepth  -17);; 
  (define CV_BadAlphaChannel  -18);; 
  (define CV_BadOrder  -19);; 
  (define CV_BadOrigin  -20);; 
  (define CV_BadAlign  -21);; 
  (define CV_BadCallBack  -22);; 
  (define CV_BadTileSize  -23);; 
  (define CV_BadCOI  -24);; 
  (define CV_BadROISize  -25);; 
  (define CV_MaskIsTiled  -26);; 
  (define CV_StsNullPtr  -27);;  null pointer 
  (define CV_StsVecLengthErr  -28);;  incorrect vector length 
  (define CV_StsFilterStructContentErr  -29);;  incorr. filter structure content 
  (define CV_StsKernelStructContentErr  -30);;  incorr. transform kernel content 
  (define CV_StsFilterOffsetErr  -31);;  incorrect filter ofset value 
  (define CV_StsBadSize  -201);;  the input/output structure size is incorrect  
  (define CV_StsDivByZero  -202);;  division by zero 
  (define CV_StsInplaceNotSupported  -203);;  in-place operation is not supported 
  (define CV_StsObjectNotFound  -204);;  request can't be completed 
  (define CV_StsUnmatchedFormats  -205);;  formats of input/output arrays differ 
  (define CV_StsBadFlag  -206);;  flag is wrong or not supported   
  (define CV_StsBadPoint  -207);;  bad CvPoint  
  (define CV_StsBadMask  -208);;  bad format of mask (neither 8uC1 nor 8sC1)
  (define CV_StsUnmatchedSizes  -209);;  sizes of input/output structures do not match 
  (define CV_StsUnsupportedFormat  -210);;  the data format/type is not supported by the function
  (define CV_StsOutOfRange  -211);;  some of parameters are out of range 
  (define CV_StsParseError  -212);;  invalid syntax/structure of the parsed file 
  (define CV_StsNotImplemented  -213);;  the requested function/feature is not implemented 
  (define CV_StsBadMemBlock  -214);;  an allocated block has been corrupted 
  (define CV_StsAssert  -215);;  assertion failed  
  (define CV_GpuNotSupported  -216);;  
  (define CV_GpuApiCallError  -217);; 
  (define CV_GpuNppCallError  -218);;
  (define CV_GpuCufftCallError  -219)

  #|************************************************************************
  *                             Common macros and inline functions          
  \************************************************************************|#
  ;; TODO : port inlined functions in case they are needed
  (define CV_PI   3.1415926535897932384626433832795)
  (define CV_LOG2 0.69314718055994530941723212145818)

  #|*************** Random number generation *******************|#
  ;; Use Racket's internal functions for rn generations

  
  #|*********************************************************************
  *                                  Image type (IplImage)              *
  \**********************************************************************|#
  
  #|*
  * The following definitions (until #endif)
  * is an extract from IPL headers.
  * Copyright (c) 1995 Intel Corporation.
  |#
  (define IPL_DEPTH_SIGN -2147483648)

  (define IPL_DEPTH_1U     1)
  (define IPL_DEPTH_8U     8)
  (define IPL_DEPTH_16U   16)
  (define IPL_DEPTH_32F   32)

  (define IPL_DEPTH_8S  (bitwise-ior IPL_DEPTH_SIGN  8))
  (define IPL_DEPTH_16S (bitwise-ior IPL_DEPTH_SIGN 16))
  (define IPL_DEPTH_32S (bitwise-ior IPL_DEPTH_SIGN 32))

  (define IPL_DATA_ORDER_PIXEL  0)
  (define IPL_DATA_ORDER_PLANE  1)

  (define IPL_ORIGIN_TL 0)
  (define IPL_ORIGIN_BL 1)

  (define IPL_ALIGN_4BYTES   4)
  (define IPL_ALIGN_8BYTES   8)
  (define IPL_ALIGN_16BYTES 16)
  (define IPL_ALIGN_32BYTES 32)

  (define IPL_ALIGN_DWORD  IPL_ALIGN_4BYTES)
  (define IPL_ALIGN_QWORD  IPL_ALIGN_8BYTES)


  (define IPL_BORDER_CONSTANT   0)
  (define IPL_BORDER_REPLICATE  1)
  (define IPL_BORDER_REFLECT    2)
  (define IPL_BORDER_WRAP       3)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Image type (IplImage)                                 
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-cstruct _IplROI
    ([coi _int] ;;0 - no COI (all channels are selected), 1 - 0th channel is selected 
     [xOffset _int]
     [yOffset _int]
     [width _int]
     [height _int]))
  
  (define-cstruct _IplImage
    ([nSize _int]
     [ID _int]
     [nChannels _int]
     [alphaChannel _int]
     [depth _int]
     [colorModel (_array _ubyte 4)]
     [channelSeq (_array _ubyte 4)]
     [dataOrder _int]
     [origin _int]
     [align _int]
     [width _int]
     [height _int]
     [roi (_cpointer _IplROI)]
     [maskROI (_cpointer _IplImage)]
     [imageId _pointer]
     [IplTileInfo _pointer]
     [imageSize _int]
     [imageData (_cpointer _ubyte)]
     [widthStep _int]
     [BorderMode (_array _int 4)]
     [BorderConst (_array _int 4)]
     [imageDataOrigin (_cpointer _ubyte)]))

  (define (IplImage-data img)
    (make-sized-byte-string (IplImage-imageData img)
                            (* (IplImage-width img)
                               (IplImage-nChannels img)
                               (IplImage-height img))))

  (define-cstruct _IplConvKernel
    ([nCols  _int]
     [nRows  _int]
     [anchorX  _int]
     [anchorY  _int]
     [values _pointer]
     [nShiftR  _int]))

  (define-cstruct _IplConvKernelFP
    ([nCols  _int]
     [nRows  _int]
     [anchorX  _int]
     [anchorY  _int]
     [values _pointer]))

  (define IPL_IMAGE_HEADER 1)
  (define IPL_IMAGE_DATA   2)
  (define IPL_IMAGE_ROI    4)

  #| extra border mode |#
  (define IPL_BORDER_REFLECT_101    4)
  (define IPL_BORDER_TRANSPARENT    5)
  (define CV_TYPE_NAME_IMAGE "opencv-image")

  #| for storing double-precision
  floating point data in IplImage's |#
  (define IPL_DEPTH_64F  64)

  #|********************** CvSlice **********************|#
  (define-cstruct _CvSlice
    ([start_index _int]
     [end_index _int]))

  (define (cvSlice start end)
    (make-CvSlice start end))

  (define CV_WHOLE_SEQ_END_INDEX #x3fffffff)
  (define CV_WHOLE_SEQ  (make-CvSlice 0 CV_WHOLE_SEQ_END_INDEX))

  #|****************************************************************
  *                       Matrix type (CvMat)                      *
  \*****************************************************************|#
  ;; TODO : port CvMat later
  (define CV_CN_MAX     512)
  (define CV_CN_SHIFT   3)
  (define CV_DEPTH_MAX  (arithmetic-shift 1 CV_CN_SHIFT))

  (define CV_8U   0)
  (define CV_8S   1)
  (define CV_16U  2)
  (define CV_16S  3)
  (define CV_32S  4)
  (define CV_32F  5)
  (define CV_64F  6)
  (define CV_USRTYPE1 7)

  (define CV_MAT_DEPTH_MASK       (- CV_DEPTH_MAX 1))
  (define (CV_MAT_DEPTH flags) (bitwise-and flags CV_MAT_DEPTH_MASK))

  (define (CV_MAKETYPE depth cn)
    (+ (CV_MAT_DEPTH depth)
       (arithmetic-shift (- cn 1) CV_CN_SHIFT)))
  
  (define CV_MAKE_TYPE CV_MAKETYPE)

  (define CV_8UC1 (CV_MAKETYPE CV_8U 1))
  (define CV_8UC2 (CV_MAKETYPE CV_8U 2))
  (define CV_8UC3 (CV_MAKETYPE CV_8U 3))
  (define CV_8UC4 (CV_MAKETYPE CV_8U 4))
  (define (CV_8UC n) CV_MAKETYPE(CV_8U n))

  (define CV_8SC1 (CV_MAKETYPE CV_8S 1))
  (define CV_8SC2 (CV_MAKETYPE CV_8S 2))
  (define CV_8SC3 (CV_MAKETYPE CV_8S 3))
  (define CV_8SC4 (CV_MAKETYPE CV_8S 4))
  (define (CV_8SC n) CV_MAKETYPE(CV_8S n))

  (define CV_16UC1 (CV_MAKETYPE CV_16U 1))
  (define CV_16UC2 (CV_MAKETYPE CV_16U 2))
  (define CV_16UC3 (CV_MAKETYPE CV_16U 3))
  (define CV_16UC4 (CV_MAKETYPE CV_16U 4))
  (define (CV_16UC n) CV_MAKETYPE(CV_16U n))

  (define CV_16SC1 (CV_MAKETYPE CV_16S 1))
  (define CV_16SC2 (CV_MAKETYPE CV_16S 2))
  (define CV_16SC3 (CV_MAKETYPE CV_16S 3))
  (define CV_16SC4 (CV_MAKETYPE CV_16S 4))
  (define (CV_16SC n) CV_MAKETYPE(CV_16S n))

  (define CV_32SC1 (CV_MAKETYPE CV_32S 1))
  (define CV_32SC2 (CV_MAKETYPE CV_32S 2))
  (define CV_32SC3 (CV_MAKETYPE CV_32S 3))
  (define CV_32SC4 (CV_MAKETYPE CV_32S 4))
  (define (CV_32SC n) CV_MAKETYPE(CV_32S n))

  (define CV_32FC1 (CV_MAKETYPE CV_32F 1))
  (define CV_32FC2 (CV_MAKETYPE CV_32F 2))
  (define CV_32FC3 (CV_MAKETYPE CV_32F 3))
  (define CV_32FC4 (CV_MAKETYPE CV_32F 4))
  (define (CV_32FC n) (CV_MAKETYPE CV_32F n))

  (define CV_64FC1 (CV_MAKETYPE CV_64F 1))
  (define CV_64FC2 (CV_MAKETYPE CV_64F 2))
  (define CV_64FC3 (CV_MAKETYPE CV_64F 3))
  (define CV_64FC4 (CV_MAKETYPE CV_64F 4))
  (define (CV_64FC n) (CV_MAKETYPE CV_64F n))

  (define CV_AUTO_STEP  #x7fffffff)
  (define CV_WHOLE_ARR  (cvSlice 0 #x3fffffff))

  (define CV_MAT_CN_MASK          (arithmetic-shift (- CV_CN_MAX 1) CV_CN_SHIFT))
  (define (CV_MAT_CN flags)
    (add1 (arithmetic-shift
           (bitwise-and flags CV_MAT_CN_MASK)
           (- CV_CN_SHIFT))))
  
  (define CV_MAT_TYPE_MASK        (sub1 (* CV_DEPTH_MAX CV_CN_MAX)))
  (define (CV_MAT_TYPE flags)      (bitwise-and flags CV_MAT_TYPE_MASK))
  (define CV_MAT_CONT_FLAG_SHIFT  14)
  (define CV_MAT_CONT_FLAG        (expt 2 CV_MAT_CONT_FLAG_SHIFT))
  (define (CV_IS_MAT_CONT flags)
    (bitwise-and flags CV_MAT_CONT_FLAG))
  (define CV_IS_CONT_MAT          CV_IS_MAT_CONT)
  (define CV_SUBMAT_FLAG_SHIFT    15)
  (define CV_SUBMAT_FLAG          (expt 2 CV_SUBMAT_FLAG_SHIFT))

  (define CV_MAGIC_MASK       #xFFFF0000)
  (define CV_MAT_MAGIC_VAL    #x42420000)
  (define CV_TYPE_NAME_MAT    "opencv-matrix")

  #| 0x3a50 = 11 10 10 01 01 00 00 ~ array of log2(sizeof(arr_type_elem)) |#
  (define (CV_ELEM_SIZE type)
    (arithmetic-shift
     (CV_MAT_CN type)
     (bitwise-and (arithmetic-shift (* (+ (/ (ctype-sizeof size_t) 4) 1)
                                       (bitwise-ior 16384 #x3a50))
                                    (- (* (CV_MAT_DEPTH type) 2)))
                  -3)))               



  (define CvMatUnion-data
    (_union _pointer    ;; byte
            _pointer    ;; s
            _pointer      ;; i
            _pointer    ;; fl 
            _pointer)) ;; db

  (define (cvMatData-ptr a-Mat (a-type _byte))
    (define ptr
      (cond [(equal? a-type _byte) 0]
            [(equal? a-type _short) 1]
            [(equal? a-type _int) 2]
            [(equal? a-type _float) 3]
            [(equal? a-type _double) 4]))
    (if (zero? ptr)
        ;; make a special case if it is a byte array
        (make-sized-byte-string (union-ref (CvMat-data a-Mat) ptr)
                                (* (CvMat-rows a-Mat)
                                   (CvMat-step a-Mat)))
        (ptr-ref
         (union-ref (CvMat-data a-Mat) ptr)
         (_array a-type (CvMat-rows a-Mat) (CvMat-cols a-Mat)))))

  (define-cstruct _CvMat
    ([type _int]
     [step _int]
     ;; for internal use only
     [refcount _pointer]
     [hdr_refcount _int]
     ;; data and dimensions
     [data CvMatUnion-data]
     [rows _int]
     [cols _int]))

  #|/**********************************************************************
  *                       Multi-dimensional dense array (CvMatND)         *
  \**********************************************************************|#
  ;; TODO : port later

  #|/**********************************************************************
  *                       Multi-dimensional sparse array (CvSparseMat)    *
  \**********************************************************************|#
  ;; TODO : port later

  #|************************************************************
  *                                         Histogram          *
  \************************************************************|#

  #|/******************************************************
  *          Other supplementary data type definitions    *
  \*******************************************************|#
  
  #|************************************ CvRect *******************|#
  (define-cstruct _CvRect
    ([x _int]
     [y _int]
     [width _int]
     [height _int]))

  (define (cvRectRoRoi rect coi)
    (make-IplROI coi
                 (CvRect-x rect)
                 (CvRect-y rect)
                 (CvRect-width rect)
                 (CvRect-height rect)))

  (define (cvROIToRect roi)
    (make-CvRect (IplROI-xOffset roi)
                 (IplROI-yOffset roi)
                 (IplROI-width roi)
                 (IplROI-height roi)))

  #|******************* CvTermCriteria ***********************************|#
  (define CV_TERMCRIT_ITER    1)
  (define CV_TERMCRIT_NUMBER  CV_TERMCRIT_ITER)
  (define CV_TERMCRIT_EPS     2)

  (define-cstruct _CvTermCriteria
    #| may be combination of
    CV_TERMCRIT_ITER
    CV_TERMCRIT_EPS |#
    ([type _int]
     [max_iter _int]
     [epsilon _double]))

  (define cvTermCriteria make-CvTermCriteria)

  #|****************** CvPoint and variants *******************************|#
  (define-cstruct _CvPoint
    ([x _int]
     [y _int]))

  (define cvPoint make-CvPoint)

  (define-cstruct _CvPoint2D32f
    ([x _float]
     [y _float]))

  (define-cstruct _CvPoint3D32f
    ([x _float]
     [y _float]
     [z _float]))
  
  (define-cstruct _CvPoint2D64f
    ([x _double]
     [y _double]))

  (define-cstruct _CvPoint3D64f
    ([x _double]
     [y _double]
     [z _double]))

  #|************************** CvSize's & CvBox *********************|#
  (define-cstruct _CvSize
    ([width _int]
     [height _int]))

  (define-cstruct _CvSize2D32f
    ([width _float]
     [height _float]))

  (define-cstruct _CvBox2D
    ([center _CvPoint2D32f] ;; Center of the box.
     [size _CvSize2D32f]    ;; Box width and length
     [angle _float]))       ;; Angle between the horizontal axis
  ;; and the first side (i.e. length) in degrees

  (define-cstruct _CvLineIterator
    ;; Pointer to the current point:
    ([ptr _pointer]
     ;; Bresenham algorithm state:
     [err _int]
     [plus_delta _int]
     [minus_delta _int]
     [plus_step _int]
     [minus_step _int]))

  #|******************** CvScalar **********************|#
  ;; TODO : make the function easier, to one that accepts 4 arguments 
  (define-cstruct _CvScalar
    ([val (_array _double 4)]))

  (define (cvScalar val0 (val1 0) (val2 0) (val3 0))
    (define an-array (ptr-ref (malloc _double 'atomic) (_array _double 4)))
    (array-set! an-array 0 (exact->inexact val0))
    (array-set! an-array 1 (exact->inexact val1))
    (array-set! an-array 2 (exact->inexact val2))
    (array-set! an-array 3 (exact->inexact val3))
    (make-CvScalar an-array))

  (define (cvRGB r g b)
    (define an-array (ptr-ref (malloc _double 'atomic) (_array _double 4)))
    (array-set! an-array 0 (exact->inexact b))
    (array-set! an-array 1 (exact->inexact g))
    (array-set! an-array 2 (exact->inexact r))
    (array-set! an-array 3 0.0)
    (make-CvScalar an-array))
  
  (define (cvRealScalar a-number)
    (define an-array (ptr-ref (malloc _double 'atomic) (_array _double 4)))
    (array-set! an-array 0 (exact->inexact a-number))
    (array-set! an-array 1 0.0)
    (array-set! an-array 2 0.0)
    (array-set! an-array 3 0.0)
    (make-CvScalar an-array))

  (define (cvScalarAll a-number)
    (define an-array (ptr-ref (malloc _double 'atomic) (_array _double 4)))
    (define inexact-number (exact->inexact a-number))
    (array-set! an-array 0 inexact-number)
    (array-set! an-array 1 inexact-number)
    (array-set! an-array 2 inexact-number)
    (array-set! an-array 3 inexact-number)
    (make-CvScalar an-array))

  #|***************************************************
  *              Dynamic Data structures              *
  \***************************************************|#

  #|******************************* Memory storage *********************|#
  (define-cstruct _CvMemBlock
    ([prev _pointer]
     [next _pointer]))
  
  (define CV_STORAGE_MAGIC_VAL    #x42890000)
  
  (define-cstruct _CvMemStorage
    ([signature _int]
     [bottom _pointer] 		#| First allocated block.                   |#
     [top _pointer] 		#| Current memory block - top of the stack. |#
     [parent _pointer] 		#| We get new blocks from parent as needed. |#
     [block_size _pointer] 	#| Block size.                              |#
     [free_space _pointer])) 	#| Remaining free space in current block.   |#

  (define-cstruct _CvMemStoragePos
    ([top _pointer]
     [free_space _int]))

  #|************************** Sequence *********************|#
  (define-cstruct _CvSeqBlock
    ([prev _pointer] #| Previous sequence block.   |#
     [next _pointer] #| Next sequence block.       |#
     [start_index _int] #| Index of the first element in the block +  |#
     #| sequence->first->start_index.              |#
     [count _int] #| Number of elements in the block.           |#
     [data _pointer])) #| Pointer to the first element of the block. |#

  #|
  Read/Write sequence.
  Elements can be dynamically inserted to or deleted from the sequence.
  |#
  (define-cstruct _CvSeq
    ([flags _int]           #| Miscellaneous flags.                 |#
     [header_size _int]     #| Size of sequence header.             |#
     [h_prev _pointer]      #| Previous sequence.                   |#
     [h_next _pointer]      #| Next sequence.                       |#
     [v_prev _pointer]      #| 2nd previous sequence.               |#
     [v_next _pointer]      #| 2nd next sequence.                   |#
     [total _int]           #| Total number of elements.            |#
     [elem_size _int]       #| Size of sequence element in bytes.   |#
     [block_max _pointer]   #| Maximal bound of the last block.     |#
     [ptr _pointer]         #| Current write pointer.               |#
     [delta_elems _int]     #| Grow seq this many at a time.        |#
     [storage _pointer]     #| Where the seq is stored.             |#
     [free_blocks _pointer] #| Free blocks list.                    |#
     [first _pointer]))     #| Pointer to the first sequence block. |#

  (define CV_TYPE_NAME_SEQ             "opencv-sequence")
  (define CV_TYPE_NAME_SEQ_TREE        "opencv-sequence-tree")

  #|************************* Set **************************|#
  #|
  Set.
  Order is not preserved. There can be gaps between sequence elements.
  After the element has been inserted it stays in the same place all the time.
  The MSB(most-significant or sign bit) of the first field (flags)
  is 0 iff the element exists.
  |#
  (define-cstruct _CvSetElem
    ([flags _int]
     [next_free _pointer]))


  #|
  Read/Write sequence.
  Elements can be dynamically inserted to or deleted from the sequence.
  |#
  (define-cstruct _CvSet
    ([flags _int]
     [header_size _int]
     [h_prev _pointer]
     [h_next _pointer]
     [v_prev _pointer]
     [v_next _pointer]
     [total _int]
     [elem_size _int]
     [block_max _pointer]
     [ptr _pointer]
     [delta_elems _int]
     [storage _int]
     [free_blocks _int]
     [first _int]
     [free_elems _pointer]
     [active_count _int]))

  ;; CV_SET_FIELDS
  ;; int       flags;             #| Miscellaneous flags.     |#      
  ;; int       header_size;       #| Size of sequence header. |#      
  ;; struct    node_type* h_prev; #| Previous sequence.       |#      
  ;; struct    node_type* h_next; #| Next sequence.           |#      
  ;; struct    node_type* v_prev; #| 2nd previous sequence.   |#      
  ;; struct    node_type* v_next  #| 2nd next sequence.       |#
  ;; int       total;          #| Total number of elements.            |#  
  ;; int       elem_size;      #| Size of sequence element in bytes.   |#  
  ;; schar*    block_max;      #| Maximal bound of the last block.     |#  
  ;; schar*    ptr;            #| Current write pointer.               |#  
  ;; int       delta_elems;    #| Grow seq this many at a time.        |#  
  ;; CvMemStorage* storage;    #| Where the seq is stored.             |#  
  ;; CvSeqBlock* free_blocks;  #| Free blocks list.                    |#  
  ;; CvSeqBlock* first;        #| Pointer to the first sequence block. |#

  (define CV_SET_ELEM_IDX_MASK   (- (expt 2 26) 1))


  #|************************************ Graph ****************************|#

  #|
  We represent a graph as a set of vertices.
  Vertices contain their adjacency lists (more exactly, pointers to first incoming or
  outcoming edge (or 0 if isolated vertex)). Edges are stored in another set.
  There is a singly-linked list of incoming/outcoming edges for each vertex.

  Each edge consists of

  o   Two pointers to the starting and ending vertices
  (vtx[0] and vtx[1] respectively).

  A graph may be oriented or not. In the latter case, edges between
  vertex i to vertex j are not distinguished during search operations.

  o   Two pointers to next edges for the starting and ending vertices, where
  next[0] points to the next edge in the vtx[0] adjacency list and
  next[1] points to the next edge in the vtx[1] adjacency list.
  |#
  (define-cstruct _CvGraphEdge
    ([flags _int]
     [weight _float]
     [next _pointer]
     [vtx _pointer]))

  (define-cstruct _CvGraphVtx
    ([flags _int]
     [first _pointer]))

  (define-cstruct _CvGraphVtx2D
    ([flags _int]
     [first _pointer]
     [ptr (_cpointer _CvPoint2D32f)]))

  #|
  Graph is "derived" from the set (this is set a of vertices)
  and includes another set (edges)
  |#
  (define-cstruct _CvGraph
    ([flags _int]
     [header_size _int]
     [h_prev _pointer]
     [h_next _pointer]
     [v_prev _pointer]
     [v_next _pointer]
     [total _int]
     [elem_size _int]
     [block_max _pointer]
     [ptr _pointer]
     [delta_elems _int]
     [storage _int]
     [free_blocks _int]
     [first _int]
     [free_elems _pointer]
     [active_count _int]
     [edges (_cpointer _CvSet)]))

  (define CV_TYPE_NAME_GRAPH "opencv-graph")



  ;;; *********************************************************
  ;;;                  Image processing types
  ;;; *********************************************************
  #| Connected component structure |#
  (define-cstruct _CvConnectedComp
    ([area _double]#| area of the connected component  |#
     [value _CvScalar] #| average color of the connected component |#
     [rect _CvRect] #| ROI of the component  |#
     [contour _pointer]))#| optional component boundary
  (the contour might have child contours corresponding to the holes)|#
  
  ;; Image smooth methods
  (define CV_BLUR_NO_SCALE 0)
  (define CV_BLUR  1)
  (define CV_GAUSSIAN  2)
  (define CV_MEDIAN 3)
  (define CV_BILATERAL 4)

  #| Filters used in pyramid decomposition |#
  (define CV_GAUSSIAN_5x5  7)

  #| Inpainting algorithms |#
  (define CV_INPAINT_NS      0)
  (define CV_INPAINT_TELEA   1)

  #| Special filters |#
  (define CV_SCHARR -1)
  (define CV_MAX_SOBEL_KSIZE 7)

  ;; Constants for color conversion
  (define CV_BGR2BGRA    0)
  (define CV_RGB2RGBA    CV_BGR2BGRA)

  (define CV_BGRA2BGR    1)
  (define CV_RGBA2RGB    CV_BGRA2BGR)

  (define CV_BGR2RGBA    2)
  (define CV_RGB2BGRA    CV_BGR2RGBA)

  (define CV_RGBA2BGR    3)
  (define CV_BGRA2RGB    CV_RGBA2BGR)

  (define CV_BGR2RGB     4)
  (define CV_RGB2BGR     CV_BGR2RGB)

  (define CV_BGRA2RGBA   5)
  (define CV_RGBA2BGRA   CV_BGRA2RGBA)

  (define CV_BGR2GRAY    6)
  (define CV_RGB2GRAY    7)
  (define CV_GRAY2BGR    8)
  (define CV_GRAY2RGB    CV_GRAY2BGR)
  (define CV_GRAY2BGRA   9)
  (define CV_GRAY2RGBA   CV_GRAY2BGRA)
  (define CV_BGRA2GRAY   10)
  (define CV_RGBA2GRAY   11)

  (define CV_BGR2BGR565  12)
  (define CV_RGB2BGR565  13)
  (define CV_BGR5652BGR  14)
  (define CV_BGR5652RGB  15)
  (define CV_BGRA2BGR565 16)
  (define CV_RGBA2BGR565 17)
  (define CV_BGR5652BGRA 18)
  (define CV_BGR5652RGBA 19)

  (define CV_GRAY2BGR565 20)
  (define CV_BGR5652GRAY 21)

  (define CV_BGR2BGR555  22)
  (define CV_RGB2BGR555  23)
  (define CV_BGR5552BGR  24)
  (define CV_BGR5552RGB  25)
  (define CV_BGRA2BGR555 26)
  (define CV_RGBA2BGR555 27)
  (define CV_BGR5552BGRA 28)
  (define CV_BGR5552RGBA 29)

  (define CV_GRAY2BGR555 30)
  (define CV_BGR5552GRAY 31)

  (define CV_BGR2XYZ     32)
  (define CV_RGB2XYZ     33)
  (define CV_XYZ2BGR     34)
  (define CV_XYZ2RGB     35)

  (define CV_BGR2YCrCb   36)
  (define CV_RGB2YCrCb   37)
  (define CV_YCrCb2BGR   38)
  (define CV_YCrCb2RGB   39)

  (define CV_BGR2HSV     40)
  (define CV_RGB2HSV     41)

  (define CV_BGR2Lab     44)
  (define CV_RGB2Lab     45)

  (define CV_BayerBG2BGR 46)
  (define CV_BayerGB2BGR 47)
  (define CV_BayerRG2BGR 48)
  (define CV_BayerGR2BGR 49)

  (define CV_BayerBG2RGB CV_BayerRG2BGR)
  (define CV_BayerGB2RGB CV_BayerGR2BGR)
  (define CV_BayerRG2RGB CV_BayerBG2BGR)
  (define CV_BayerGR2RGB CV_BayerGB2BGR)

  (define CV_BGR2Luv     50)
  (define CV_RGB2Luv     51)
  (define CV_BGR2HLS     52)
  (define CV_RGB2HLS     53)

  (define CV_HSV2BGR     54)
  (define CV_HSV2RGB     55)

  (define CV_Lab2BGR     56)
  (define CV_Lab2RGB     57)
  (define CV_Luv2BGR     58)
  (define CV_Luv2RGB     59)
  (define CV_HLS2BGR     60)
  (define CV_HLS2RGB     61)

  (define CV_BayerBG2BGR_VNG 62)
  (define CV_BayerGB2BGR_VNG 63)
  (define CV_BayerRG2BGR_VNG 64)
  (define CV_BayerGR2BGR_VNG 65)
  
  (define CV_BayerBG2RGB_VNG CV_BayerRG2BGR_VNG)
  (define CV_BayerGB2RGB_VNG CV_BayerGR2BGR_VNG)
  (define CV_BayerRG2RGB_VNG CV_BayerBG2BGR_VNG)
  (define CV_BayerGR2RGB_VNG CV_BayerGB2BGR_VNG)
  
  (define CV_BGR2HSV_FULL  66)
  (define CV_RGB2HSV_FULL  67)
  (define CV_BGR2HLS_FULL  68)
  (define CV_RGB2HLS_FULL  69)
  
  (define CV_HSV2BGR_FULL  70)
  (define CV_HSV2RGB_FULL  71)
  (define CV_HLS2BGR_FULL  72)
  (define CV_HLS2RGB_FULL  73)
  
  (define CV_LBGR2Lab      74)
  (define CV_LRGB2Lab      75)
  (define CV_LBGR2Luv      76)
  (define CV_LRGB2Luv      77)
  
  (define CV_Lab2LBGR      78)
  (define CV_Lab2LRGB      79)
  (define CV_Luv2LBGR      80)
  (define CV_Luv2LRGB      81)
  
  (define CV_BGR2YUV       82)
  (define CV_RGB2YUV       83)
  (define CV_YUV2BGR       84)
  (define CV_YUV2RGB       85)
  
  (define CV_BayerBG2GRAY  86)
  (define CV_BayerGB2GRAY  87)
  (define CV_BayerRG2GRAY  88)
  (define CV_BayerGR2GRAY  89)

  (define CV_YUV420i2RGB   90)
  (define CV_YUV420i2BGR   91)
  (define CV_YUV420sp2RGB  92)
  (define CV_YUV420sp2BGR  93)
  
  (define CV_COLORCVT_MAX  100)

  #| Sub-pixel interpolation methods |#
  (define CV_INTER_NN        0)
  (define CV_INTER_LINEAR    1)
  (define CV_INTER_CUBIC     2)
  (define CV_INTER_AREA      3)
  (define CV_INTER_LANCZOS4  4)

  #| ... and other image warping flags |#
  (define CV_WARP_FILL_OUTLIERS 8)
  (define CV_WARP_INVERSE_MAP  16)

  #| Shapes of a structuring element for morphological operations |#
  (define CV_SHAPE_RECT      0)
  (define CV_SHAPE_CROSS     1)
  (define CV_SHAPE_ELLIPSE   2)
  (define CV_SHAPE_CUSTOM    100)

  #| Morphological operations |#
  (define CV_MOP_ERODE        0)
  (define CV_MOP_DILATE       1)
  (define CV_MOP_OPEN         2)
  (define CV_MOP_CLOSE        3)
  (define CV_MOP_GRADIENT     4)
  (define CV_MOP_TOPHAT       5)
  (define CV_MOP_BLACKHAT     6)

  #| Spatial and central moments |#
  (define-cstruct _CvMoments
    ;; spatial moments
    ([m00 _double][m10 _double][m01 _double][m20 _double]
     [m11 _double][m02 _double][m30 _double][m21 _double]
     [m12 _double][m03 _double]
     ;; central moments
     [mu20 _double][mu11 _double][mu02 _double][mu30 _double]
     [mu21 _double][mu12 _double][mu03 _double]
     [inv_sqrt_m00 _double])) ;; m00 != 0 ? 1/sqrt(m00) : 0

  #| Hu invariants |#
  (define-cstruct _CvHuMoments
    #| Hu invariants |#
    ([hu1 _double]
     [hu2 _double]
     [hu3 _double]
     [hu4 _double]
     [hu5 _double]
     [hu6 _double]
     [hu7 _double]))

  #| Template matching methods |#
  (define CV_TM_SQDIFF         0)
  (define CV_TM_SQDIFF_NORMED  1)
  (define CV_TM_CCORR          2)
  (define CV_TM_CCORR_NORMED   3)
  (define CV_TM_CCOEFF         4)
  (define CV_TM_CCOEFF_NORMED  5)

  (define _CvDistanceFunction
    (_fun _pointer _pointer _pointer -> _float))

  #| Contour retrieval modes |#
  (define CV_RETR_EXTERNAL 0)
  (define CV_RETR_LIST 1)
  (define CV_RETR_CCOMP 2)
  (define CV_RETR_TREE 3)

  #| Contour approximation methods |#
  (define CV_CHAIN_CODE 0)
  (define CV_CHAIN_APPROX_NONE 1)
  (define CV_CHAIN_APPROX_SIMPLE 2)
  (define CV_CHAIN_APPROX_TC89_L1 3)
  (define CV_CHAIN_APPROX_TC89_KCOS 4)
  (define CV_LINK_RUNS 5)

  #|*
  Internal structure that is used for sequental retrieving contours from the image.
  It supports both hierarchical and plane variants of Suzuki algorithm.
  |#
  (define _CvContourScanner _pointer)

  ;; CV_SEQ_WRITER_FIELDS
  ;; [header_size _int] 
  ;; [seq _pointer] #| the sequence written |#
  ;; [block _pointer] #| current block |#
  ;; [ptr _pointer] #| pointer to free space |#
  ;; [block_min _pointer] #| pointer to the beginning of block|#
  ;; [header_block_max _pointer]#| pointer to the end of block |#

  #| Freeman chain reader state |#
  (define-cstruct _CvChainPtReader
    ([header_size _int] 
     [seq _pointer] #| the sequence written |#
     [block _pointer] #| current block |#
     [ptr _pointer] #| pointer to free space |#
     [block_min _pointer] #| pointer to the beginning of block|#
     [header_block_max _pointer]#| pointer to the end of block |#
     [code _ubyte]
     [pt _CvPoint]
     [deltas (_array (_array _sbyte 2) 8)]))

  ;; TODO : port this code if needed
  ;; (define (CV_INIT_3x3_DELTAS deltas step nch)
  ;;   (set! ))
  #| initializes 8-element array for fast access to 3x3 neighborhood of a pixel |#
  ;; #define  CV_INIT_3X3_DELTAS( deltas, step, nch )            \
  ;;     ((deltas)[0] =  (nch),  (deltas)[1] = -(step) + (nch),  \
  ;;      (deltas)[2] = -(step), (deltas)[3] = -(step) - (nch),  \
  ;;      (deltas)[4] = -(nch),  (deltas)[5] =  (step) - (nch),  \
  ;;      (deltas)[6] =  (step), (deltas)[7] =  (step) + (nch))

  #|***************************************************************************
  *                              Planar subdivisions                          *
  *****************************************************************************|#
  (define size_t _uint)
  (define _CvSubdiv2DEdge size_t)

  ;;CV_QUADEDGE2D_FIELDS()
  ;; [flags _int]
  ;; [pt (_array _CvSubdiv2DPoint 4)]
  ;; [next (_array _CvSubdiv2DEdge 4)]

  ;; CV_SUBDIV2D_POINT_FIELDS()
  ;; [flags _int]
  ;; [first _CvSubdiv2DEdge]
  ;; [pt _CvPoint2D32f]
  ;; [id _int]

  (define CV_SUBDIV2D_VIRTUAL_POINT_FLAG (expt 2 30))

  (define-cstruct _CvSubdiv2DPoint
    ([flags _int]
     [first _CvSubdiv2DEdge]
     [pt _CvPoint2D32f]
     [id _int]))

  (define-cstruct _CvQuadEdge2D
    ([flags _int]
     [pt (_array _CvSubdiv2DPoint 4)]
     [next (_array _CvSubdiv2DEdge 4)]))


  (define-cstruct _CvSubdiv2D
    ([flags _int]
     [pt (_array _CvSubdiv2DPoint 4)]
     [next (_array _CvSubdiv2DEdge 4)]
     [quad_edges _int]
     [is_geometry_valid _int]
     [recent_edge _CvSubdiv2DEdge]
     [topleft  _CvPoint2D32f]
     [bottomright  _CvPoint2D32f]))


  (define _CvSubdiv2DPointLocation
    (_enum
     '(CV_PTLOC_ERROR = -2
                      CV_PTLOC_OUTSIDE_RECT = -1
                      CV_PTLOC_INSIDE = 0
                      CV_PTLOC_VERTEX = 1
                      CV_PTLOC_ON_EDGE = 2)))
  
  (define _CvNextEdgeType
    (_enum
     '(CV_NEXT_AROUND_ORG   = #x00
                            CV_NEXT_AROUND_DST   = #x22
                            CV_PREV_AROUND_ORG   = #x11
                            CV_PREV_AROUND_DST   = #x33
                            CV_NEXT_AROUND_LEFT  = #x13
                            CV_NEXT_AROUND_RIGHT = #x31
                            CV_PREV_AROUND_LEFT  = #x20
                            CV_PREV_AROUND_RIGHT = #x02)))

  ;; TODO : implement in Racket if necessary
  #| get the next edge with the same origin point (counterwise) |#
  ;; #(define  CV_SUBDIV2D_NEXT_EDGE( edge )  (((CvQuadEdge2D*)((edge) & ~3))->next[(edge)&3])

  #| Contour approximation algorithms |#
  (define CV_POLY_APPROX_DP 0)

  #| Shape matching methods |#
  (define CV_CONTOURS_MATCH_I1  1)
  (define CV_CONTOURS_MATCH_I2  2)
  (define CV_CONTOURS_MATCH_I3  3)

  #| Shape orientation |#
  (define CV_CLOCKWISE         1)
  (define CV_COUNTER_CLOCKWISE 2)


  #| Convexity defect |#
  (define-cstruct _CvConvexityDefect
    #| point of the contour where the defect begins |#
    ([start _pointer]
     #| point of the contour where the defect ends |#
     [end (_cpointer _CvPoint)] 
     #| the farthest from the convex hull point within the defect |#
     [depth_point (_cpointer _CvPoint)]
     #| distance between the farthest point and the convex hull |#
     [depth _float]))

  #| Histogram comparison methods |#
  (define CV_COMP_CORREL        0)
  (define CV_COMP_CHISQR        1)
  (define CV_COMP_INTERSECT     2)
  (define CV_COMP_BHATTACHARYYA 3)

  #| Mask size for distance transform |#
  (define CV_DIST_MASK_3   3)
  (define CV_DIST_MASK_5   5)
  (define CV_DIST_MASK_PRECISE 0)

  #| Distance types for Distance Transform and M-estimators |#
  (define CV_DIST_USER    -1)  #| User defined distance |#
  (define CV_DIST_L1      1)   #| distance = |x1-x2| + |y1-y2| |#
  (define CV_DIST_L2      2)   #| the simple euclidean distance |#
  (define CV_DIST_C       3)   #| distance = max(|x1-x2||y1-y2|) |#
  (define CV_DIST_L12     4)   #| L1-L2 metric: distance = 2(sqrt(1+x*x/2) - 1)) |#
  (define CV_DIST_FAIR    5)   #| distance = c^2(|x|/c-log(1+|x|/c)) c = 1.3998 |#
  (define CV_DIST_WELSCH  6)   #| distance = c^2/2(1-exp(-(x/c)^2)) c = 2.9846 |#
  (define CV_DIST_HUBER   7)    #| distance = |x|<c ? x^2/2 : c(|x|-c/2) c=1.345 |#



  #| Threshold types |#
  (define CV_THRESH_BINARY      0)  #| value = value > threshold ? max_value : 0 |#
  (define CV_THRESH_BINARY_INV  1)  #| value = value > threshold ? 0 : max_value  |#
  (define CV_THRESH_TRUNC       2)  #| value = value > threshold ? threshold : value|#
  (define CV_THRESH_TOZERO      3)  #| value = value > threshold ? value : 0  |#
  (define CV_THRESH_TOZERO_INV  4)  #| value = value > threshold ? 0 : value  |#
  (define CV_THRESH_MASK        7)
  (define CV_THRESH_OTSU        8)  #| use Otsu algorithm to choose
  the optimal threshold value
  combine the flag with one of the above CV_THRESH_* values |#


  #| Adaptive threshold methods |#
  (define CV_ADAPTIVE_THRESH_MEAN_C  0)
  (define CV_ADAPTIVE_THRESH_GAUSSIAN_C  1)


  #| FloodFill flags |#
  (define CV_FLOODFILL_FIXED_RANGE (expt 2 16))
  (define CV_FLOODFILL_MASK_ONLY   (expt 2 17))



  #| Canny edge detector flags |#
  (define CV_CANNY_L2_GRADIENT  (- (expt 2 31)))


  #| Variants of a Hough transform |#
  (define CV_HOUGH_STANDARD 0)
  (define CV_HOUGH_PROBABILISTIC 1)
  (define CV_HOUGH_MULTI_SCALE 2)
  (define CV_HOUGH_GRADIENT 3)



  #|*************************************************************************
  * Data structures for persistence (a.k.a serialization) functionality 
  **************************************************************************|#

  ;; Storage flags:
  (define CV_STORAGE_READ          0)
  (define CV_STORAGE_WRITE         1)
  (define CV_STORAGE_WRITE_TEXT    CV_STORAGE_WRITE)
  (define CV_STORAGE_WRITE_BINARY  CV_STORAGE_WRITE)
  (define CV_STORAGE_APPEND        2)
  (define CV_STORAGE_MEMORY        4)
  (define CV_STORAGE_FORMAT_MASK   (arithmetic-shift 7 3))
  (define CV_STORAGE_FORMAT_AUTO   0)
  (define CV_STORAGE_FORMAT_XML    8)
  (define CV_STORAGE_FORMAT_YAML  16)

  ;; List of attributes:
  (define-cstruct _CvAttrList
    ;; NULL-terminated array of (attribute_name,attribute_value) pairs.
    ([attr _pointer]
     ;; Pointer to next chunk of the attributes list.
     [next _pointer]))

  (define (cvAttrList (attr #f) (next #f))
    (make-CvAttrList attr next))
  
  )
