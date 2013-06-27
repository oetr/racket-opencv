;; Author: Petr Samarin
;; Description: Porting imgproc_c.h to Racket

(module core racket
  (provide (all-defined-out))
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define
           "core.rkt"
           "types.rkt")
  
  (define-ffi-definer define-opencv-imgproc
    (ffi-lib "/opt/local/lib/libopencv_imgproc"))

  (define-syntax (defimgproc stx)
    (syntax-case stx ()
      [(defimgproc name #:ptr type)
       #`(begin (provide name)
                (define name
                  (let ()
                    (define-opencv-imgproc ptr _pointer
                      #:c-id #,(datum->syntax
                                #'name
                                (string->symbol
                                 (format "_~a" (syntax->datum #'name)))))
                    (function-ptr ptr type))))]
      [(defimgproc name type)
       #'(begin (provide name)
                (define-opencv-imgproc name type))]))   


  ;; TODO: describe functions with comments, not names?
  ;; *************** Background statistics accumulation ***********
  ;;  Adds image to accumulator
  (define-opencv-imgproc cvAcc
    (_fun (image : _pointer) (sum : _pointer) (mask : _pointer)
          -> _void))

  ;;  Adds squared image to accumulator
  (define-opencv-imgproc cvSquareAcc
    (_fun (image : _pointer) (sqsum : _pointer) (mask : _pointer)
          -> _void))

  ;;  Adds a product of two images to accumulator
  (define-opencv-imgproc cvMultiplyAcc
    (_fun (image1 : _pointer) (image2 : _pointer) (acc : _pointer)
          (mask : _pointer)
          -> _void))

  ;;  Adds image to accumulator with weights:
  ;; acc = acc*(1-alpha) + image*alpha
  (define-opencv-imgproc cvRunningAvg
    (_fun (image : _pointer) (acc : _pointer) (alpha : _double)
          (mask : _pointer)
          -> _void))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Image processing procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Copies source 2D array inside of the larger
  ;; destination array and makes a border of the
  ;; specified type (IPL_BORDER_*) around the copied area.
  (define-opencv-imgproc cvCopyMakeBorder
    (_fun (src : _pointer) (dst : _pointer) (offset : _CvPoint) (bordertype : _int)
          (value : _CvScalar) -> _void))

  ;; Smoothes array (removes noise)
  (define-opencv-imgproc cvSmooth
    (_fun
     (src dst (smoothtype CV_GAUSSIAN) (size1 3) (size2 0) (sigma1 0.0) (sigma2 0.0))
     ::
     (src : _pointer)
     (dst : _pointer)
     (smoothtype : _int)
     (size1 : _int)
     (size2 : _int)
     (sigma1 : _double)
     (sigma2 : _double)
     -> _void))

  ;; Convolves the image with the kernel 
  (define-opencv-imgproc cvFilter2D
    (_fun (src : _pointer) (dst : _pointer) (kernel : _pointer)
          (anchor : _CvPoint) -> _void))

  ;; Finds integral image: SUM(X,Y) = sum(x<X,y<Y)I(x,y)
  (define-opencv-imgproc cvIntegral
    (_fun (image : _pointer) (sum : _pointer) (sqsum : _pointer)
          (tilted_sum : _pointer) -> _void))

  ;; Smoothes the input image with gaussian kernel and then down-samples it.  
  ;; dst_width = floor(src_width/2)[+1],
  ;; dst_height = floor(src_height/2)[+1]
  (define-opencv-imgproc cvPyrDown
    (_fun (src dst (filter CV_GAUSSIAN_5x5)) ::
          (src : _pointer)
          (dst : _pointer)
          (filter : _int)
          -> _void))

  ;; a nicer version of pyrDown taken from the OpenCV book
  ;; doPyrDown : IplImage x number -> IplImage
  (define (doPyrDown an-image (a-filter CV_GAUSSIAN_5x5))
    ;; get image parameters dependent on image type
    (define width 0)
    (define height 0)    
    (cond
     [(IplImage? an-image)
      (set! width (IplImage-width an-image))
      (set! height (IplImage-height an-image))]
     [(CvMat? an-image)
      (set! width (CvMat-cols an-image))
      (set! height (CvMat-rows an-image))])
    ;; make sure that the image is divisible by 2
    (unless (and (zero? (modulo width 2))
                 (zero? (modulo height 2)))      
      (error
       'doPyrDown
       "image dimensions must be divisible by 2, but got width=~a, height=~a~n"
       width height))
    ;; create an image of the same type
    (cond
     [(IplImage? an-image)
      (define out-image (cvCreateImage (make-CvSize (/ width 2)
                                                    (/ height 2))
                                       (IplImage-depth an-image)
                                       (IplImage-nChannels an-image)))
      (cvPyrDown an-image out-image a-filter)
      out-image]
     [(CvMat? an-image)
      (define out-image
        (cvMat (/ height 2) (/ width 2)
               (CvMat-type an-image)))
      (cvPyrDown an-image out-image a-filter)
      out-image]))

  (define (doPyrDown! an-image (a-filter CV_GAUSSIAN_5x5))
    (define pyred-down-image (doPyrDown an-image a-filter))
    (cond [(IplImage? an-image) (cvReleaseImage an-image)]
          [(CvMat? an-image) (cvReleaseMat an-image)])
    pyred-down-image)

  ;; Up-samples image and smoothes the result with gaussian kernel.
  ;; dst_width = src_width*2,
  ;; dst_height = src_height*2
  (define-opencv-imgproc cvPyrUp
    (_fun (src : _pointer) (dst : _pointer) (filter : _int)
          -> _void))

  ;; Builds pyramid for an image
  ;; TODO: it returns CvMat**, test it!
  (define-opencv-imgproc cvCreatePyramid
    (_fun (img : _pointer) (extra_layers : _int) (rate : _double)
          (layer_sizes : _pointer) (bufarr : _pointer) (calc : _int)
          (filter : _int)
          -> (_ptr o (_ptr o _pointer))))

  ;; Releases pyramid
  (define-opencv-imgproc cvReleasePyramid
    (_fun (pyramid : _pointer) (extra_layers : _pointer)
          -> _void))  

  #| Splits color or grayscale image into multiple connected components
  of nearly the same color/brightness using modification of Burt algorithm.
  comp with contain a pointer to sequence (CvSeq)
  of connected components (CvConnectedComp) |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvPyrSegmentation
  ;;   (_fun _pointer _pointer _pointer _pointer _int _double _double
  ;;         -> _void))
  
  #| Filters image using meanshift algorithm |#
  (define-opencv-imgproc cvPyrMeanShiftFiltering
    (_fun _pointer _pointer _double _double _int _CvTermCriteria
          -> _void))

  #| Segments image using seed "markers" |#
  (define-opencv-imgproc cvWatershed
    (_fun _pointer _pointer -> _void))

  #| Inpaints the selected region in the image |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvInpaint
  ;;   (_fun _pointer _pointer _pointer _double _int -> _void))

  #| Calculates an image derivative using generalized Sobel
  (aperture_size = 1,3,5,7) or Scharr (aperture_size = -1) operator.
  Scharr can be used only for the first dx or dy derivative |#
  (define-opencv-imgproc cvSobel
    (_fun _pointer _pointer _int _int _int -> _void))

  #| Calculates the image Laplacian: (d2/dx + d2/dy)I |#
  (define-opencv-imgproc cvLaplace
    (_fun _pointer _pointer _int -> _void))

  #| Converts input array pixels from one color space to another |#
  (define-opencv-imgproc cvCvtColor
    (_fun _pointer _pointer _int -> _void))

  #| A version of cvtColor that creates the destination image internally |#
  (define (cvt-color src code dst-cn)
    (define dst (cvCreateMat (CvMat-rows src)
                             (CvMat-cols src)
                             dst-cn))
    (cvCvtColor src dst code)
    dst)

  (define (convert-color an-image new-depth new-channels conversion-flag)
    (define out-image (cvCreateImage
                       (make-CvSize (IplImage-width an-image)
                                    (IplImage-height an-image))
                       new-depth
                       new-channels))
    (cvCvtColor an-image out-image conversion-flag)
    out-image)

  #| Resizes image (input array is resized to fit the destination array) |#
  (define-opencv-imgproc cvResize
    (_fun _pointer _pointer _int -> _void))

  #| Warps image with affine transform |#
  (define-opencv-imgproc cvWarpAffine
    (_fun (src dst map-matrix
               (flags (+ CV_INTER_LINEAR CV_WARP_FILL_OUTLIERS))
               (fillval (cvScalarAll 0))) ::
               (src : _pointer)
               (dst : _pointer)
               (map-matrix : _pointer)
               (flags : _int)
               (fillval : _CvScalar)
               -> _void))

  #| Computes affine transform matrix for mapping src[i] to dst[i] (i=0,1,2) |#
  (define-opencv-imgproc cvGetAffineTransform
    (_fun (src dst (map-matrix (cvCreateMat 2 3 CV_32FC1))) ::
          (src : _pointer)
          (dst : _pointer)
          (map-matrix : _pointer)
          -> (result : _pointer)
          -> (ptr-ref result _CvMat)))

  #| Computes rotation_matrix matrix |#
  (define-opencv-imgproc cv2DRotationMatrix
    (_fun (center angle scale (map-matrix (cvCreateMat 2 3 CV_32FC1))) ::
          (center : _CvPoint2D32f)
          (angle : _double)
          (scale : _double)
          (map-matrix : _pointer)
          -> (result : _pointer)
          -> (ptr-ref result _CvMat)))
          

  #| Warps image with perspective (projective) transform |#
  (define-opencv-imgproc cvWarpPerspective
    (_fun (src dst map-matrix
               (flags (+ CV_INTER_LINEAR CV_WARP_FILL_OUTLIERS))
               (fillval (cvScalarAll 0))) ::
               (src : _pointer)
               (dst : _pointer)
               (map-matrix : _pointer)
               (flags : _int)
               (fillval : _CvScalar)
               -> _void))

  #| Computes perspective transform matrix for mapping src[i] to dst[i] (i=0,1,2,3) |#
  (define-opencv-imgproc cvGetPerspectiveTransform
    (_fun (src dst (map-matrix (cvCreateMat 3 3 CV_32FC1))) ::
          (src : _pointer)
          (dst : _pointer)
          (map-matrix : _pointer)
          -> (_ptr o _CvMat)))
  
  #| Performs generic geometric transformation using the specified coordinate maps |#
  (define-opencv-imgproc cvRemap
    (_fun _pointer _pointer _pointer _pointer _int _CvScalar -> _void))

  #| Converts mapx & mapy from floating-point to integer formats for cvRemap |#
  (define-opencv-imgproc cvConvertMaps
    (_fun _pointer _pointer _pointer _pointer -> _void))

  #| Performs forward or inverse log-polar image transform |#
  (define-opencv-imgproc cvLogPolar
    (_fun _pointer _pointer _CvPoint2D32f _double _int -> _void))

  #| Performs forward or inverse linear-polar image transform |#
  (define-opencv-imgproc cvLinearPolar
    (_fun _pointer _pointer _CvPoint2D32f _double _int -> _void))

  #| Transforms the input image to compensate lens distortion |#
  (define-opencv-imgproc cvUndistort2
    (_fun _pointer _pointer _pointer _pointer _pointer -> _void))
  
  #| Computes transformation map from intrinsic camera parameters
  that can used by cvRemap |#
  (define-opencv-imgproc cvInitUndistortMap
    (_fun _pointer _pointer _pointer _pointer -> _void))

  #| Computes undistortion+rectification map for a head of stereo camera |#
  (define-opencv-imgproc cvInitUndistortRectifyMap
    (_fun _pointer _pointer _pointer _pointer _pointer _pointer -> _void))

  #| Computes the original (undistorted) feature coordinates
  from the observed (distorted) coordinates |#
  (define-opencv-imgproc cvUndistortPoints
    (_fun _pointer _pointer _pointer _pointer _pointer _pointer -> _void))

  #| creates structuring element used for morphological operations |#
  (define-opencv-imgproc cvCreateStructuringElementEx
    (_fun _int _int _int _int _int _pointer -> _pointer))

  #| releases structuring element |#
  (define-opencv-imgproc cvReleaseStructuringElement
    (_fun _pointer -> _void))

  #| erodes input image (applies minimum filter) one or more times.
  If element pointer is NULL, 3x3 rectangular element is used |#  
  (define-opencv-imgproc cvErode
    (_fun (src dst (element #f) (iterations 1)) ::
          (src : _pointer)
          (dst : _pointer)
          (element : _pointer)
          (iterations : _int)
          -> _void))

  #| dilates input image (applies maximum filter) one or more times.
  If element pointer is NULL, 3x3 rectangular element is used |#
  (define-opencv-imgproc cvDilate
    (_fun _pointer _pointer _pointer _int -> _void))

  #| Performs complex morphological transformation |#
  (define-opencv-imgproc cvMorphologyEx
    (_fun _pointer _pointer _pointer _pointer _int _int -> _void))

  #| Calculates all spatial and central moments up to the 3rd order |#
  (define-opencv-imgproc cvMoments
    (_fun _pointer _pointer _pointer _int -> _void))

  #| Retrieve particular spatial, central or normalized central moments |#
  (define-opencv-imgproc cvGetSpatialMoment
    (_fun _pointer _int _int -> _double))
  (define-opencv-imgproc cvGetCentralMoment
    (_fun _pointer _int _int -> _double))
  (define-opencv-imgproc cvGetNormalizedCentralMoment
    (_fun _pointer _int _int -> _double))

  #| Calculates 7 Hu's invariants from precalculated
  spatial and central moments |#
  (define-opencv-imgproc cvGetHuMoments
    (_fun _pointer _pointer -> _void))

  #|************************** data sampling ***********************|#

  #| Fetches pixels that belong to the specified
  line segment and stores them to the buffer.
  Returns the number of retrieved points. |#
  (define-opencv-imgproc cvSampleLine
    (_fun _pointer _CvPoint _CvPoint _pointer _int -> _void))

  #| Retrieves the rectangular image region with specified center from the input array.
  dst(x,y) <- src(x + center.x - dst_width/2, y + center.y - dst_height/2).
  Values of pixels with fractional coordinates are retrieved using bilinear
  interpolation|#
  (define-opencv-imgproc cvGetRectSubPix
    (_fun _pointer _pointer _CvPoint2D32f -> _void))
 
  #| Retrieves quadrangle from the input array.
  matrixarr = ( a11  a12 | b1 )   dst(x,y) <- src(A[x y]' + b)
  ( a21  a22 | b2 ) (bilinear interpolation is used to retrieve pixels
  with fractional coordinates) |#
  (define-opencv-imgproc cvGetQuadrangleSubPix
    (_fun _pointer _pointer _pointer -> _void))

  #| Measures similarity between template and overlapped windows in the source image
  and fills the resultant image with the measurements |#
  (define-opencv-imgproc cvMatchTemplate
    (_fun _pointer _pointer _pointer _int -> _void))

  #| Computes earth mover distance between
  two weighted point sets (called signatures) |#
  (define-opencv-imgproc cvCalcEMD2
    (_fun _pointer _pointer _int _CvDistanceFunction _pointer _pointer
          _pointer _pointer -> _float))

  #|****************************************************
  *                              Contours retrieving   *
  ****************************************************** |#

  #| Retrieves outer and optionally inner boundaries of white (non-zero) connected
  components in the black (zero) background |#  
  (define-opencv-imgproc cvFindContours
    (_fun (image storage first-contour (header-size (ctype-sizeof _CvContour))
                 (mode CV_RETR_LIST) (method CV_CHAIN_APPROX_SIMPLE)
                 (offset (cvPoint 0 0))) ::
                 [image : _pointer]
                 [storage : _pointer]
                 [first-contour : _pointer]
                 [header-size : _int]
                 [mode : _int]
                 [method : _int]
                 [offset : _CvPoint]
                 -> _int))

  #| Initalizes contour retrieving process.
  Calls cvStartFindContours.
  Calls cvFindNextContour until null pointer is returned
  or some other condition becomes true.
  Calls cvEndFindContours at the end. |#
  (define-opencv-imgproc cvStartFindContours
    (_fun (image storage
                 (header-size (ctype-sizeof _CvContour))
                 (mode CV_RETR_LIST)
                 (method CV_CHAIN_APPROX_SIMPLE)
                 (offset (cvPoint 0 0))) ::
          [image : _pointer]
          [storage : _pointer]
          [header-size : _int]
          [mode : _int]
          [method : _int]
          [offset : _CvPoint]
          -> _CvContourScanner))

  #| Retrieves next contour |#
   (define-opencv-imgproc cvFindNextContour
     (_fun _CvContourScanner -> _pointer))

  #| Substitutes the last retrieved contour with the new one
  (if the substitutor is null, the last retrieved contour is removed from the tree) |#
   (define-opencv-imgproc cvSubstituteContour
     (_fun _CvContourScanner _pointer -> _void))

  #| Releases contour scanner and returns pointer to the first outer contour |#
  (define-opencv-imgproc cvEndFindContours
    (_fun  _pointer -> _void))

  #| Approximates a single Freeman chain or a tree of chains to polygonal curves |#
  (define-opencv-imgproc cvApproxChains
    (_fun  _pointer _pointer _int _double _int _int -> _void))

  #| Initalizes Freeman chain reader.
  The reader is used to iteratively get coordinates of all the chain points.
  If the Freeman codes should be read as is, a simple sequence reader should be used |#
  (define-opencv-imgproc cvStartReadChainPoints
    (_fun  _pointer _pointer -> _void))

  #| Retrieves the next chain point |#
  (define-opencv-imgproc cvReadChainPoint
    (_fun  _pointer -> _CvPoint))


  #|************************ high-level subdivision functions ************|#

  #| Simplified Delaunay diagram creation |#
  ;; (define-opencv-imgproc cvCreateSubdivDelaunay2D
  ;;   (_fun _CvRect _pointer -> _pointer))
  ;; TODO: inline the function:  
  ;;   CV_INLINE  CvSubdiv2D* cvCreateSubdivDelaunay2D( CvRect rect, CvMemStorage* storage )
  ;;   {
  ;;     CvSubdiv2D* subdiv = cvCreateSubdiv2D( CV_SEQ_KIND_SUBDIV2D, sizeof(*subdiv),
  ;;                          sizeof(CvSubdiv2DPoint), sizeof(CvQuadEdge2D), storage );

  ;;     cvInitSubdivDelaunay2D( subdiv, rect );
  ;;     return subdiv;
  ;; }


  #| Inserts new point to the Delaunay triangulation |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvSubdivDelaunay2DInsert
  ;;   (_fun _pointer _CvPoint2D32f -> _pointer))

  #| Locates a point within the Delaunay triangulation (finds the edge
  the point is left to or belongs to, or the triangulation point the given
  point coinsides with |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvSubdiv2DLocate
  ;;   (_fun _pointer _CvPoint2D32f _pointer _pointer  -> _CvSubdiv2DPointLocation))

  #| Calculates Voronoi tesselation (i.e. coordinates of Voronoi points) |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvCalcSubdivVoronoi2D
  ;;   (_fun _pointer  -> _void))

  #| Removes all Voronoi points from the tesselation |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvClearSubdivVoronoi2D
  ;;   (_fun _pointer  -> _void))

  #| Finds the nearest to the given point vertex in subdivision. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvFindNearestPoint2D
  ;;   (_fun _pointer _CvPoint2D32f  -> _pointer))
  
  #|*********** Basic quad-edge navigation and operations ***********|#
  ;; TODO: inline following function in Racket
  ;; CV_INLINE  CvSubdiv2DEdge  cvSubdiv2DNextEdge( CvSubdiv2DEdge edge )
  ;; {
  ;;     return  CV_SUBDIV2D_NEXT_EDGE(edge);
  ;; }


  ;; CV_INLINE  CvSubdiv2DEdge  cvSubdiv2DRotateEdge( CvSubdiv2DEdge edge, int rotate )
  ;; {
  ;;     return  (edge & ~3) + ((edge + rotate) & 3);
  ;; }

  ;; CV_INLINE  CvSubdiv2DEdge  cvSubdiv2DSymEdge( CvSubdiv2DEdge edge )
  ;; {
  ;;     return edge ^ 2;
  ;; }

  ;; CV_INLINE  CvSubdiv2DEdge  cvSubdiv2DGetEdge( CvSubdiv2DEdge edge, CvNextEdgeType type )
  ;; {
  ;;     CvQuadEdge2D* e = (CvQuadEdge2D*)(edge & ~3);
  ;;     edge = e->next[(edge + (int)type) & 3];
  ;;     return  (edge & ~3) + ((edge + ((int)type >> 4)) & 3);
  ;; }


  ;; CV_INLINE  CvSubdiv2DPoint*  cvSubdiv2DEdgeOrg( CvSubdiv2DEdge edge )
  ;; {
  ;;     CvQuadEdge2D* e = (CvQuadEdge2D*)(edge & ~3);
  ;;     return (CvSubdiv2DPoint*)e->pt[edge & 3];
  ;; }


  ;; CV_INLINE  CvSubdiv2DPoint*  cvSubdiv2DEdgeDst( CvSubdiv2DEdge edge )
  ;; {
  ;;     CvQuadEdge2D* e = (CvQuadEdge2D*)(edge & ~3);
  ;;     return (CvSubdiv2DPoint*)e->pt[(edge + 2) & 3];
  ;; }


  ;; CV_INLINE  double  cvTriangleArea( CvPoint2D32f a, CvPoint2D32f b, CvPoint2D32f c )
  ;; {
  ;;     return ((double)b.x - a.x) * ((double)c.y - a.y) - ((double)b.y - a.y) * ((double)c.x - a.x);
  ;; }


  #|************************************************************************\
  *                            Contour Processing and Shape Analysis       *
  \*************************************************************************|#
  #| Approximates a single polygonal curve (contour) or
  a tree of polygonal curves (contours) |#
  (define-opencv-imgproc cvApproxPoly
    (_fun _pointer _int _pointer _int _double _int -> _pointer))
  
  #| Calculates perimeter of a contour or length of a part of contour |#
  (define-opencv-imgproc cvArcLength
    (_fun _pointer _CvSlice _int -> _double))

  ;; TODO: inline
  ;; CV_INLINE double cvContourPerimeter( const void* contour )
  ;; {
  ;;     return cvArcLength( contour, CV_WHOLE_SEQ, 1 );
  ;; }


  #| Calculates contour boundning rectangle (update=1) or
  just retrieves pre-calculated rectangle (update=0) |#
  (define-opencv-imgproc cvBoundingRect
    (_fun (points (update 0)) ::
          (points : _pointer)
          (update : _int)
          -> _CvRect))

  #| Calculates area of a contour or contour segment |#
  (define-opencv-imgproc cvContourArea
    (_fun _pointer _CvSlice _int -> _double))

  #| Finds minimum area rotated rectangle bounding a set of points |#
  (define-opencv-imgproc cvMinAreaRect2
    (_fun (points (storage #f)) ::
          (points : _pointer)
          (storage : _pointer)
          -> _CvBox2D))

  #| Finds minimum enclosing circle for a set of points |#
  (define-opencv-imgproc cvMinEnclosingCircle
    (_fun _pointer _pointer _pointer -> _int))
  
  #| Compares two contours by matching their moments |#
  (define-opencv-imgproc cvMatchShapes
    (_fun _pointer _pointer _int _double -> _double))

  #| Calculates exact convex hull of 2d point set |#  
  (define-opencv-imgproc cvConvexHull2
    (_fun (input (hull-storage (cvCreateMemStorage 0))
                 (orientation CV_CLOCKWISE) (return-points 0)) ::
                 (input : _pointer)
                 (hull-storage : _pointer)
                 (orientation : _int)
                 (return-points : _int)
                 -> (_ptr o _CvSeq)))

  #| Checks whether the contour is convex or not (returns 1 if convex, 0 if not) |#
  (define-opencv-imgproc cvCheckContourConvexity
    (_fun _pointer -> _int))

  #| Finds convexity defects for the contour |#
  (define-opencv-imgproc cvConvexityDefects
    (_fun _pointer _pointer _pointer -> _pointer))

  #| Fits ellipse into a set of 2d points |#
  (define-opencv-imgproc cvFitEllipse2
    (_fun _pointer -> _CvBox2D))

  #| Finds minimum rectangle containing two given rectangles |#
  (define-opencv-imgproc cvMaxRect
    (_fun _pointer _pointer -> _CvRect))

  #| Finds coordinates of the box vertices |#
  (define-opencv-imgproc cvBoxPoints
    (_fun (box) ::
          (box : _CvBox2D)
          (pt : (_list o _CvPoint2D32f 4))
          -> _void
          -> pt))

  #| Initializes sequence header for a matrix (column or row vector) of points -
  a wrapper for cvMakeSeqHeaderForArray
  (it does not initialize bounding rectangle!!!) |#
  (define-opencv-imgproc cvPointSeqFromMat
    (_fun _int _pointer _pointer _pointer -> _pointer))

  #| Checks whether the point is inside polygon, outside, on an edge (at a vertex).
  Returns positive, negative or zero value, correspondingly.
  Optionally, measures a signed distance between
  the point and the nearest polygon edge (measure_dist=1) |#
  (define-opencv-imgproc cvPointPolygonTest
    (_fun _pointer _CvPoint2D32f _int -> _double))

  #|**************************************************************************\
  *                                  Histogram functions                     *
  \***************************************************************************|#
  #| Creates new histogram |#
  (define-opencv-imgproc cvCreateHist
    (_fun _int _pointer _int _pointer _int -> _pointer))

  #| Assignes histogram bin ranges |#
  (define-opencv-imgproc cvSetHistBinRanges
    (_fun _pointer _pointer _int -> _void))

  #| Creates histogram header for array |#
  (define-opencv-imgproc cvMakeHistHeaderForArray
    (_fun _int _pointer _pointer _pointer _pointer _int -> _pointer))

  #| Releases histogram |#
  (define-opencv-imgproc cvReleaseHist
    (_fun _pointer -> _void))

  #| Clears all the histogram bins |#
  (define-opencv-imgproc cvClearHist
    (_fun _pointer -> _void))

  #| Finds indices and values of minimum and maximum histogram bins |#
  (define-opencv-imgproc cvGetMinMaxHistValue
    (_fun _pointer _pointer _pointer _pointer _pointer -> _void))


  #| Normalizes histogram by dividing all bins by sum of the bins, multiplied by <factor>.
  After that sum of histogram bins is equal to <factor> |#
  (define-opencv-imgproc cvNormalizeHist
    (_fun _pointer _double -> _void))


  #| Clear all histogram bins that are below the threshold |#
  (define-opencv-imgproc cvThreshHist
    (_fun _pointer _double -> _void))


  #| Compares two histogram |#
  (define-opencv-imgproc cvCompareHist
    (_fun _pointer _pointer _int -> _double))

  #| Copies one histogram to another. Destination histogram is created if
  the destination pointer is NULL |#
  (define-opencv-imgproc cvCopyHist
    (_fun _pointer _pointer -> _void))


  #| Calculates bayesian probabilistic histograms
  (each or src and dst is an array of <number> histograms |#
  (define-opencv-imgproc cvCalcBayesianProb
    (_fun _pointer _int _pointer -> _void))

  #| Calculates array histogram |#
  (define-opencv-imgproc cvCalcArrHist
    (_fun _pointer _pointer _int _pointer -> _void))

  ;; TODO : inline
  ;; CV_INLINE  void  cvCalcHist( IplImage** image, CvHistogram* hist,
  ;;                                         int accumulate CV_DEFAULT(0),
  ;;                                         const CvArr* mask CV_DEFAULT(NULL) )
  ;; {
  ;;  cvCalcArrHist( (CvArr**)image, hist, accumulate, mask );
  ;;               }

  #| Calculates back project |#
  (define-opencv-imgproc cvCalcArrBackProject
    (_fun _pointer _pointer _pointer -> _void))

  ;; TODO : write a Racket procedure for the code:  
  ;;#define  cvCalcBackProject(image, dst, hist) cvCalcArrBackProject((CvArr**)image, dst, hist)

  #| Does some sort of template matching but compares histograms of
  template and each window location |#
  (define-opencv-imgproc cvCalcArrBackProjectPatch
    (_fun _pointer _pointer _CvSize _pointer _int _double -> _void))

  ;; TODO: Racket procedure
  ;;#define  cvCalcBackProjectPatch( image, dst, range, hist, method, factor ) \
  ;;cvCalcArrBackProjectPatch( (CvArr**)image, dst, range, hist, method, factor )

  #| calculates probabilistic density (divides one histogram by another) |#
  (define-opencv-imgproc cvCalcProbDensity
    (_fun _pointer _pointer _pointer _double -> _void))

  #| equalizes histogram of 8-bit single-channel image |#
  (define-opencv-imgproc cvEqualizeHist
    (_fun _pointer _pointer -> _void))


  #| Applies distance transform to binary image |#
  (define-opencv-imgproc cvDistTransform
    (_fun _pointer _int _int _pointer _pointer -> _void))


  #| Applies fixed-level threshold to grayscale image.
  This is a basic operation applied before retrieving contours |#
  (define-opencv-imgproc cvThreshold
    (_fun _pointer _pointer _double _double _int -> _double))

  #| Applies adaptive threshold to grayscale image.
  The two parameters for methods CV_ADAPTIVE_THRESH_MEAN_C and
  CV_ADAPTIVE_THRESH_GAUSSIAN_C are:
  neighborhood size (3, 5, 7 etc.),
  and a constant subtracted from mean (...,-3,-2,-1,0,1,2,3,...) |#
  (define-opencv-imgproc cvAdaptiveThreshold
    (_fun _pointer _pointer _double _int _int _int _double -> _void))

  #| Fills the connected component until the color difference gets large enough |#
  (define-opencv-imgproc cvFloodFill
    (_fun _pointer _CvPoint _CvScalar _CvScalar _CvScalar _pointer _int _pointer
          -> _void))

  #|*************************************************************************\
  *                                  Feature detection                      *
  \**************************************************************************|#

  #| Runs canny edge detector |#
  (define-opencv-imgproc cvCanny
    (_fun _pointer _pointer _double _double _int -> _void))

  #| Calculates constraint image for corner detection
  Dx^2 * Dyy + Dxx * Dy^2 - 2 * Dx * Dy * Dxy.
  Applying threshold to the result gives coordinates of corners |#
  (define-opencv-imgproc cvPreCornerDetect
    (_fun _pointer _pointer _int -> _void))

  #| Calculates eigen values and vectors of 2x2
  gradient covariation matrix at every image pixel |#
  (define-opencv-imgproc cvCornerEigenValsAndVecs
    (_fun _pointer _pointer _int _int -> _void))

  #| Calculates minimal eigenvalue for 2x2 gradient covariation matrix at
  every image pixel |#
  (define-opencv-imgproc cvCornerMinEigenVal
    (_fun _pointer _pointer _int _int -> _void))

  #| Harris corner detector:
  Calculates det(M) - k*(trace(M)^2), where M is 2x2 gradient covariation matrix for each pixel |#
  (define-opencv-imgproc cvCornerHarris
    (_fun _pointer _pointer _int _int _double -> _void))

  #| Adjust corner position using some sort of gradient search |#
  (define-opencv-imgproc cvFindCornerSubPix
    (_fun _pointer _pointer _int _CvSize _CvSize _CvTermCriteria -> _void))

  #| Finds a sparse set of points within the selected region
  that seem to be easy to track |#
  (define-opencv-imgproc cvGoodFeaturesToTrack
    (_fun (image eig-image temp-image corners corner-count quality-level min-distance
                 (mask #f) (block-size 3) (use-harris 0) (k 0.04)) ::
                 (image         : (_ptr i _CvMat))
                 (eig-image     : _pointer)
                 (temp-image    : _pointer)
                 (corners       : _pointer)
                 (corner-count  : _pointer)
                 (quality-level : _double)
                 (min-distance  : _double)
                 (mask          : _pointer)
                 (block-size    : _int)
                 (use-harris    : _int)
                 (k             : _double)
                 -> _void))

  #| Finds lines on binary image using one of several methods.
  line_storage is either memory storage or 1 x <max number of lines> CvMat, its
  number of columns is changed by the function.
  method is one of CV_HOUGH_*;
  rho, theta and threshold are used for each of those methods;
  param1 ~ line length, param2 ~ line gap - for probabilistic,
  param1 ~ srn, param2 ~ stn - for multi-scale |#
  (define-opencv-imgproc cvHoughLines2
    (_fun _pointer _pointer _int _double _double _int _double _double  -> _void))

  #| Finds circles in the image |#
  (define-opencv-imgproc cvHoughCircles
    (_fun _pointer _pointer _int _double _double _double _double _int _int  -> _void))

  #| Fits a line into set of 2d or 3d points in a robust way (M-estimator technique)|#
  (define-opencv-imgproc cvFitLine
    (_fun _pointer _int _double _double _double _pointer -> _void))


  #| Constructs kd-tree from set of feature descriptors |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvCreateKDTree
  ;;   (_fun _pointer -> _pointer))

  #| Constructs spill-tree from set of feature descriptors |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvCreateSpillTree
  ;;   (_fun _pointer _int _double _double -> _pointer))

  #| Release feature tree |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvReleaseFeatureTree
  ;;   (_fun _pointer -> _void))

  #| Searches feature tree for k nearest neighbors of given reference points,
  searching (in case of kd-tree/bbf) at most emax leaves. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvFindFeatures
  ;;   (_fun _pointer _pointer _pointer _pointer _int _int -> _void))

  #| Search feature tree for all points that are inlier to given rect region.
  Only implemented for kd trees |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvFindFeaturesBoxed
  ;;   (_fun _pointer _pointer _pointer _pointer -> _int))


  #| Construct a Locality Sensitive Hash (LSH) table, for indexing d-dimensional vectors of
  given type. Vectors will be hashed L times with k-dimensional p-stable (p=2) functions. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvCreateLSH
  ;;   (_fun _pointer _int _int _int _int _double _int64  -> _pointer))

  #| Construct in-memory LSH table, with n bins. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvCreateMemoryLSH
  ;;   (_fun _int _int _int _int _int _double _int64 -> _pointer))

  #| Free the given LSH structure. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvReleaseLSH
  ;;   (_fun _pointer -> _void))

  #| Return the number of vectors in the LSH. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc LSHSize
  ;;   (_fun _pointer -> _uint))

  #| Add vectors to the LSH structure, optionally returning indices. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvLSHAdd
  ;;   (_fun _pointer _pointer _pointer -> _void))

  #| Remove vectors from LSH, as addressed by given indices. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvLSHRemove
  ;;   (_fun _pointer _pointer -> _void))

  #| Query the LSH n times for at most k nearest points; data is n x d,
  indices and dist are n x k. At most emax stored points will be accessed. |#
  ;; Not available in version 2.4.0
  ;; (define-opencv-imgproc cvLSHQuery
  ;;   (_fun _pointer _pointer _pointer _pointer _int  _int -> _void))
  )
