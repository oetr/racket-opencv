(module highgui racket
  (provide (all-defined-out))
;;; Libraries
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define)
  ;; Ported OpenCV requirements
  (require "types.rkt")
  ;; Unit Testing
  (require rackunit)

;;; FFI Constants
  ;; These 3 flags are used by cvSet/GetWindowProperty
  ;; to change/get window's fullscreen property
  (define CV_WND_PROP_FULLSCREEN 0)
  ;; to change/get window's autosize property
  (define CV_WND_PROP_AUTOSIZE    1)
  ;; to change/get window's aspectratio property
  (define CV_WND_PROP_ASPECTRATIO 2)

  ;; These 2 flags are used by cvNamedWindow and cvSet/GetWindowProperty
  ;; the user can resize the window (no constraint)  / also use to switch a fullscreen window to a normal size
  (define CV_WINDOW_NORMAL        #x00000000)
  ;; the user cannot resize the window) the size is constrainted by the image displayed
  (define CV_WINDOW_AUTOSIZE      #x00000001)

  ;; Those flags are only for Qt
  ;; status bar and tool bar
  (define CV_GUI_EXPANDED          #x00000000)
  ;; old fashious way
  (define CV_GUI_NORMAL            #x00000010)

  ;; These 3 flags are used by cvNamedWindow and cvSet/GetWindowProperty
  ;; change the window to fullscreen
  (define CV_WINDOW_FULLSCREEN    1)
  ;; the image expends as much as it can (no ratio constraint)
  (define CV_WINDOW_FREERATIO     #x00000100)
  ;; the ration image is respected.
  (define CV_WINDOW_KEEPRATIO     #x00000000)

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



;;; Matrix
  (define data (_union (_cpointer _ubyte)
                       (_cpointer _short)
                       (_cpointer _int)
                       (_cpointer _float)
                       (_cpointer _double)))

  (define-cstruct _CvMat
    ([type _int]
     [step _int]
     ;; for internal use only
     [refcount _gcpointer]
     [hdr_refcount _int]
     [rows _int]
     [cols _int]))

  (define-opencv-highgui cvCreateMat (_fun _int _int _int -> _pointer))


  )