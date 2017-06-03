;; Author: Peter Samarin

#lang racket/base

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         rackunit
         ;; OpenCV requirements
         "types.rkt"
         "core.rkt"
         "imgproc.rkt"
         "videoio.rkt"
         "utilities.rkt")

(provide (all-defined-out))


(define opencv-objdetect-lib
  (case (system-type)
    [(windows)
     (ffi-lib
      (build-path (system-library-subpath #f)
                  "libopencv_objdetect246"))]
    [(macosx) (ffi-lib "/opt/local/lib/libopencv_objdetect")]
    [else (ffi-lib "libopencv_objdetect")]))

(define-ffi-definer define-opencv-objdetect-internal opencv-objdetect-lib)

(define-syntax define-opencv-objdetect
  (syntax-rules ()
    [(_ name body)
     (begin
       (provide name)
       (define-opencv-objdetect-internal name body))]))

#|***********************************************************
*           Haar-like Object Detection functions
*************************************************************
|#
(define CV_HAAR_MAGIC_VAL    #x42500000)
(define CV_TYPE_NAME_HAAR    "opencv-haar-classifier")

;; TODO : define later
;; (define (CV_IS_HAAR_CLASSIFIER haar)
;;   (and (not (= haar #f))

;;   (((const CvHaarClassifierCascade*)(haar))->flags & CV_MAGIC_MASK)==CV_HAAR_MAGIC_VAL)
;; (define-opencv-highgui cvLoadHaarClassifierCascade
;;   (_fun _pointer _CvSize -> _pointer))

(define CV_HAAR_FEATURE_MAX  3)

(define-cstruct _internal_rect
  ([r _CvRect]
   [weight _float]))

(define-cstruct _CvHaarFeature
  ([tilted _int]
   [rect  (_array _internal_rect CV_HAAR_FEATURE_MAX)]))

(define-cstruct _CvHaarClassifier
  ([count _int]
   [haar_feature _CvHaarFeature-pointer/null]
   [threshold (_cpointer _float)]
   [left (_cpointer _int)]
   [right (_cpointer _int)]
   [alpha (_cpointer _float)]))

(define-cstruct _CvHaarStageClassifier
  ([count _int]
   [threshold _float]
   [classifier _CvHaarClassifier-pointer/null]
   [next _int]
   [child _int]
   [parent _int]))

;; (define-cstruct _CvHaarStageClassifier
;;   ([count _int]
;;    [threshold _float]
;;    [classifier (_cpointer _CvHaarClassifier)]
;;    [next _int]
;;    [child _int]
;;    [parent _int]))

(define-cstruct _CvHaarClassifierCascade
  ([flags _int]
   [count _int]
   [orig_window_size _CvSize]
   [real_window_size _CvSize]
   [scale _double]
   [stage_classifier _CvHaarStageClassifier-pointer/null]
   [hid_cascade _CvHaarClassifierCascade-pointer/null]))

(define-cstruct _CvAvgComp
  ([rect _CvRect]
   [neighbors _int]))

#| Loads haar classifier cascade from a directory.
It is obsolete: convert your cascade to xml and use cvLoad instead |#
(define-opencv-objdetect cvLoadHaarClassifierCascade
  (_fun _path _CvSize ->
        (classifier : (_ptr io _CvHaarClassifierCascade))
        -> (ptr-ref classifier _CvHaarClassifierCascade)))

(define-opencv-objdetect cvReleaseHaarClassifierCascade
  (_fun _pointer -> _void))

(define CV_HAAR_DO_CANNY_PRUNING    1)
(define CV_HAAR_SCALE_IMAGE         2)
(define CV_HAAR_FIND_BIGGEST_OBJECT 4)
(define CV_HAAR_DO_ROUGH_SEARCH     8)

(define-opencv-objdetect cvHaarDetectObjects
  (_fun (image cascade storage (scale_factor 1.1) (min_neighbors 3) (flags 0)
               (min_size (make-CvSize 0 0))
               (max_size (make-CvSize 0 0))) ::
               [image : _pointer]
               [cascade : _pointer]
               [storage : _pointer]
               [scale_factor : _double]
               [min_neighbors : _int]
               [flags : _int]
               [min_size : _CvSize]
               [max_size : _CvSize]
               -> (sequence : (_ptr io _CvSeq))
               -> (ptr-ref sequence _CvSeq)))

#| sets images for haar classifier cascade |#
(define-opencv-objdetect cvSetImagesForHaarClassifierCascade
  (_fun _pointer _pointer _pointer _pointer _double -> _void))


#| runs the cascade on the specified window |#
(define-opencv-objdetect cvRunHaarClassifierCascade
  (_fun (cascade pt (start_stage 0)) ::
        [cascade : _pointer]
        [pt : _CvPoint]
        [start_stage : _int]
        -> _int))

#|********************************************************************************
*                       Latent SVM Object Detection functions           
*********************************************************************************|#
;; DataType: STRUCT position
;; Structure describes the position of the filter in the feature pyramid
;; l - level in the feature pyramid
;; (x, y) - coordinate in level l
(define-cstruct _CvLSVMFilterPosition
  ([x _int]
   [y _int]
   [l _int]))

;; DataType: STRUCT filterObject
;; Description of the filter, which corresponds to the part of the object
;; V               - ideal (penalty = 0) position of the partial filter
;;                   from the root filter position (V_i in the paper)
;; penaltyFunction - vector describes penalty function (d_i in the paper)
;;                   pf[0] * x + pf[1] * y + pf[2] * x^2 + pf[3] * y^2
;; FILTER DESCRIPTION
;;   Rectangular map (sizeX x sizeY), 
;;   every cell stores feature vector (dimension = p)
;; H               - matrix of feature vectors
;;                   to set and get feature vectors (i,j) 
;;                   used formula H[(j * sizeX + i) * p + k], where
;;                   k - component of feature vector in cell (i, j)
;; END OF FILTER DESCRIPTION
(define-cstruct _CvLSVMFilterObject
  ([V _CvLSVMFilterPosition]
   [fineFunction (_array _float 4)]
   [sizeX _int]
   [sizeY _int]
   [numFeatures _int]
   [H (_cpointer _float)]))

;; TODO: figure out where it went in the new version of opencv
;; data type: STRUCT CvLatentSvmDetector
;; structure contains internal representation of trained Latent SVM detector
;; num_filters	- total number of filters (root plus part) in model 
;; num_components	- number of components in model
;; num_part_filters	- array containing number of part filters for each component
;; filters		- root and part filters for all model components
;; b			- biases for all model components
;; score_threshold	- confidence level threshold
;; (define-cstruct _CvLatentSvmDetector
;;   ([num_filters _int]
;;    [num_components _int]
;;    [num_part_filters _pointer]
;;    [filters _pointer]
;;    [b _pointer]
;;    [score_threshold _float]))

;; data type: STRUCT CvObjectDetection
;; structure contains the bounding box and confidence level for detected object 
;; rect					- bounding box for a detected object
;; score				- confidence level
(define-cstruct _CvObjectDetection
  ([rect _CvRect]
   [score _float]))


  ;;;;;;;;;;;;;;;; Object Detection using Latent SVM ;;;;;;;;;;;;;;
#|
;; load trained detector from a file
;;
;; API
;; CvLatentSvmDetector* cvLoadLatentSvmDetector(const char* filename);
;; INPUT
;; filename	- path to the file containing the parameters of
;; 		- trained Latent SVM detector
;; OUTPUT
;; trained Latent SVM detector in internal representation
|#
;; (define-opencv-objdetect cvLoadLatentSvmDetector
;;   (_fun _string -> _pointer))
;; -> (svm-detector : (_ptr io _CvLatentSvmDetector))
;; -> (ptr-ref svm-detector _CvLatentSvmDetector)))

#|
;; release memory allocated for CvLatentSvmDetector structure
;;
;; API
;; void cvReleaseLatentSvmDetector(CvLatentSvmDetector** detector);
;; INPUT
;; detector		- CvLatentSvmDetector structure to be released
;; OUTPUT
|#
;; (define-opencv-objdetect cvReleaseLatentSvmDetector
;;   (_fun _pointer -> _void))

#|
;; find rectangular regions in the given image that are likely 
;; to contain objects and corresponding confidence levels
;;
;; API
;; CvSeq* cvLatentSvmDetectObjects(const IplImage* image, 
;;			CvLatentSvmDetector* detector, 
;;			CvMemStorage* storage, 
;;			float overlap_threshold = 0.5f,
;;                    int numThreads = -1);
;; INPUT
;; image		- image to detect objects in
;; detector		- Latent SVM detector in internal representation
;; storage		- memory storage to store the resultant sequence 
;;		          of the object candidate rectangles
;; overlap_threshold	- threshold for the non-maximum suppression algorithm 
;; 			= 0.5f [here will be the reference to original paper]
;; OUTPUT
;; sequence of detected objects (bounding boxes and confidence levels stored in
;; CvObjectDetection structures)
|#
;; (define-opencv-objdetect cvLatentSvmDetectObjects
;;   (_fun (image detector storage (overlap_threshold 0.5) (numThreads -1)) ::
;;         [image : _pointer] ;; IplImage*
;;         [detector : _pointer]
;;         [storage : _pointer]
;;         [overlap_threshold : _float]
;;         [numThreads : _int]
;;         -> _pointer))

#|********************************************************
**                Datamatrix                
**********************************************************
|#
(define-cstruct _CvDataMatrixCode
  ([msg (_array _ubyte 4)]
   [original (_cpointer _CvMat)]
   [corners (_cpointer _CvMat)]))

#|***************************************************************************
**         LINE-MOD               
*****************************************************************************|#
;;/ @todo Convert doxy comments to rst

#|*
* \brief Discriminant feature described by its location and label.
|#
;; (define-cstruct _Feature
;;   ([x _int]
;;    [y _int]
;;    [label _int]
;;    []
;;    [original (_cpointer _CvMat)]
;;    [corners (_cpointer _CvMat)])

