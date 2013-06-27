;; Author: Petr Samarin
;; Description: Porting callib3d.hpp to Racket

(module core racket
  (provide (all-defined-out))
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define
           "core.rkt"
           "types.rkt")
  
  (define-ffi-definer define-opencv-calib3d
    (ffi-lib "/opt/local/lib/libopencv_calib3d"))

  #|* Allocates and initializes CvPOSITObject structure before doing cvPOSIT |#
  (define-opencv-calib3d cvCreatePOSITObject
    (_fun _pointer _int -> _pointer))

  #| Runs POSIT (POSe from ITeration) algorithm for determining 3d position of
   an object given its model and projection in a weak-perspective case |#
  (define-opencv-calib3d cvPOSIT
    (_fun _pointer _pointer _double _CvTermCriteria _pointer _pointer -> _void))

  #| Finds extrinsic camera parameters from
   a few known corresponding point pairs and intrinsic parameters |#
  (define-opencv-calib3d cvFindExtrinsicCameraParams2
    (_fun (object-points image-points camera-matrix distortion-coeffs rotation-vector
                         translation-vector (use-extrinsic-guess 0)) ::
                         [object-points : (_ptr i _CvMat)]
                         [image-points : (_ptr i _CvMat)]
                         [camera-matrix : (_ptr i _CvMat)]
                         [distortion-coeffs :(_ptr i _CvMat)]
                         [rotation-vector : (_ptr o _CvMat)]
                         [translation-vector : (_ptr o _CvMat)]
                         [use-extrinsic-guess : _int]
                         -> _void))

  ;; Converts rotation vector to rotation matrix or vice versa
  (define-opencv-calib3d cvRodrigues2
    (_fun (src dst (jacobian #f)) ::
          [src : (_ptr i _CvMat)]
          [dst : _pointer]
          [jacobian : _pointer] ->
          _void))

  )
