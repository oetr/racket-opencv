(module types racket
  (provide (all-defined-out))

  (require ffi/unsafe
           ffi/unsafe/define)
  
  (define-ffi-definer define-opencv-highgui
    (ffi-lib "/opt/local/lib/libopencv_highgui"))

  (define CvArr _void)

  (define-cstruct _CvScalar
    ([val (_array _double 4)]))

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

  (define CV_PI   3.1415926535897932384626433832795)
  (define CV_LOG2 0.69314718055994530941723212145818)

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
  (define-cstruct _CvPoint
    ([x _int]
     [y _int]))
  
  (define-cstruct _CvSize
    ([width _int]
     [height _int]))
  
    (define-cstruct _CvRect
    ([x _int]
     [y _int]
     [width _int]
     [height _int]))

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

  ;; (define-cstruct _CvMat
  ;;   ([type  _int]
  ;;    [step  _int]
  ;;    [refcount  (_cpointer _int)]
  ;;    [hdr_refcount  _int]
  ;;    [values _pointer]))

  )

;; typedef struct CvMat
;; {
;;     int type;
;;     int step;

;;     /* for internal use only */
;;     int* refcount;
;;     int hdr_refcount;

;;     union
;;     {
;;         uchar* ptr;
;;         short* s;
;;         int* i;
;;         float* fl;
;;         double* db;
;;     } data;
;;     int rows;
;;     int cols;

;; }
;; CvMat;