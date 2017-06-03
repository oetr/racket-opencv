;; Author: Peter Samarin

#lang racket/base

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         ;; OpenCV requirements
         "types.rkt"
         "core.rkt"
         "imgproc.rkt"
;;         "imgcodecs.rkt"
         "videoio.rkt"
         "utilities.rkt")

(provide (all-from-out "types.rkt")
         (all-from-out "core.rkt")
         (all-from-out "imgproc.rkt")
         (all-from-out "videoio.rkt"))

(define opencv-highgui-lib
  (case (system-type)
    [(windows)
     (ffi-lib
      (build-path (system-library-subpath #f)
                  "libopencv_highgui246"))]
    [(macosx) (ffi-lib "/opt/local/lib/libopencv_highgui")]
    [else (ffi-lib "libopencv_highgui")]))

(define-ffi-definer define-opencv-highgui-internal opencv-highgui-lib)

(define-syntax define-opencv-highgui
  (syntax-rules ()
    [(_ name body)
     (begin
       (provide name)
       (define-opencv-highgui-internal name body))]))

;; this function is used to set some external parameters in case of X Window
(define-opencv-highgui cvInitSystem
  (_fun _int (_ptr i (_ptr i _ubyte))
        -> (r : _int)
        -> (check-return r 'cvInitSystem)))

(define-opencv-highgui cvStartWindowThread
  (_fun -> (r : _int) -> (check-return r 'cvInitSystem)))

;; ----- YV -------
;;These 3 flags are used by cvSet/GetWindowProperty
(define+provide CV_WND_PROP_FULLSCREEN  0) ;; to change/get window's fullscreen property
(define+provide CV_WND_PROP_AUTOSIZE    1) ;; to change/get window's autosize property
(define+provide CV_WND_PROP_ASPECTRATIO 2) ;; to change/get window's aspectratio property

;; These 2 flags are used by cvNamedWindow and cvSet/GetWindowProperty
(define+provide CV_WINDOW_NORMAL        #x00000000) ;; the user can resize the window (no constraint)  / also use to switch a fullscreen window to a normal size
(define+provide CV_WINDOW_AUTOSIZE      #x00000001) ;; the user cannot resize the window, the size is constrainted by the image displayed

;; Those flags are only for Qt
(define+provide CV_GUI_EXPANDED          #x00000000) ;; status bar and tool bar
(define+provide CV_GUI_NORMAL            #x00000010) ;; old fashious way

;; These 3 flags are used by cvNamedWindow and cvSet/GetWindowProperty
(define+provide CV_WINDOW_FULLSCREEN    1) ;; change the window to fullscreen
(define+provide CV_WINDOW_FREERATIO     #x00000100) ;; the image expends as much as it can (no ratio constraint)
(define+provide CV_WINDOW_KEEPRATIO     #x00000000) ;; the ration image is respected.

;; Create a window
(define-opencv-highgui cvNamedWindow
  (_fun (name (flags CV_WINDOW_AUTOSIZE)) ::
        (name : _string)
        (flags : _int)
        -> (r : _int)
        -> (check-return r 'cvNamedWindow)))

;; Set and Get Property of the window
(define-opencv-highgui cvSetWindowProperty
  (_fun _string _int _double -> _void))

(define-opencv-highgui cvGetWindowProperty
  (_fun _string _int -> _double))

;; display image within window
;; (highgui windows remember their content)
(define-opencv-highgui cvShowImage
  (_fun _string _pointer -> _void))

(define+provide imshow cvShowImage)

;; resize/move window
(define-opencv-highgui cvResizeWindow
  (_fun _string _int _int -> _void))
(define-opencv-highgui cvMoveWindow
  (_fun _string _int _int -> _void))

;; destroy window and all the trackers associated with it
(define-opencv-highgui cvDestroyWindow
  (_fun _string -> _void))

(define-opencv-highgui cvDestroyAllWindows
  (_fun -> _void))


;; get native window handle (HWND in case of Win32 and Widget in case of X Window)
(define-opencv-highgui cvGetWindowHandle
  (_fun _string -> _pointer))

;; get name of highgui window given its native handle */
(define-opencv-highgui cvGetWindowName
  (_fun _pointer -> _pointer))

;;(define+provide CvTrackbarCallback (_fun _int -> _void))

#| create trackbar and display it on top of given window, set callback |#
(define-opencv-highgui cvCreateTrackbar
  (_fun (trackbar-name window-name value count (on-change #f)) ::
        [trackbar-name : _string]
        [window-name : _string]
        [value : (_ptr o _int)]
        [count : _int]
        [on-change : (_fun _int -> _void)]
        -> (r : _int)
        -> (check-return r 'cvCreateTrackbar)))

(define+provide CvTrackbarCallback2 (_fun _int _pointer -> _void))

(define-opencv-highgui cvCreateTrackbar2
  (_fun _string _string _pointer _int CvTrackbarCallback2 _pointer
        -> (r : _int)
        -> (check-return r 'cvCreateTrackbar2)))

;; retrieve or set trackbar position
(define-opencv-highgui cvGetTrackbarPos
  (_fun _string _string -> _int))

(define-opencv-highgui cvSetTrackbarPos
  (_fun _string _string _int -> _void))

(define+provide CV_EVENT_MOUSEMOVE      0)
(define+provide CV_EVENT_LBUTTONDOWN    1)
(define+provide CV_EVENT_RBUTTONDOWN    2)
(define+provide CV_EVENT_MBUTTONDOWN    3)
(define+provide CV_EVENT_LBUTTONUP      4)
(define+provide CV_EVENT_RBUTTONUP      5)
(define+provide CV_EVENT_MBUTTONUP      6)
(define+provide CV_EVENT_LBUTTONDBLCLK  7)
(define+provide CV_EVENT_RBUTTONDBLCLK  8)
(define+provide CV_EVENT_MBUTTONDBLCLK  9)

(define+provide CV_EVENT_FLAG_LBUTTON   1)
(define+provide CV_EVENT_FLAG_RBUTTON   2)
(define+provide CV_EVENT_FLAG_MBUTTON   4)
(define+provide CV_EVENT_FLAG_CTRLKEY   8)
(define+provide CV_EVENT_FLAG_SHIFTKEY  16)
(define+provide CV_EVENT_FLAG_ALTKEY    3)

(define+provide CvMouseCallback (_fun _int _int _int _int _pointer -> _void))
;;  assign callback for mouse events
(define-opencv-highgui cvSetMouseCallback
  (_fun _string CvMouseCallback _pointer
        -> (r : _int)
        -> (check-return r 'cvSetMouseCallback)))

;; 8bit, color or not
(define+provide CV_LOAD_IMAGE_UNCHANGED  -1)
;; 8bit, gray
(define+provide CV_LOAD_IMAGE_GRAYSCALE  0)
;; ?, color
(define+provide CV_LOAD_IMAGE_COLOR      1)
;; any depth, ?
(define+provide CV_LOAD_IMAGE_ANYDEPTH   2)
;; ?, any color
(define+provide CV_LOAD_IMAGE_ANYCOLOR   4)

#| load image from file
iscolor can be a combination of above flags where CV_LOAD_IMAGE_UNCHANGED
overrides the other flags
using CV_LOAD_IMAGE_ANYCOLOR alone is equivalent to CV_LOAD_IMAGE_UNCHANGED
unless CV_LOAD_IMAGE_ANYDEPTH is specified images are converted to 8bit
|#  
(define-opencv-highgui cvLoadImage
  (_fun (filename (iscolor CV_LOAD_IMAGE_COLOR)) ::
        (filename : _string)
        (iscolor  : _int)
        -> (r : (_ptr io _IplImage))
        -> (ptr-ref r _IplImage)))

(define-opencv-highgui cvLoadImageM
  (_fun (filename (iscolor CV_LOAD_IMAGE_COLOR)) ::
        (filename : _string)
        (iscolor  : _int)
        -> (r : (_ptr o _CvMat))))

(define+provide imread cvLoadImageM)

(define+provide CV_IMWRITE_JPEG_QUALITY 1)
(define+provide CV_IMWRITE_PNG_COMPRESSION 16)
(define+provide CV_IMWRITE_PXM_BINARY 32)

;; save image to file
;; the last argument is a pointer to an integer array
;; of size of 3:
;; 1) one of the three constants above
;; 2) compression value
;; 3) no idea
(define-opencv-highgui cvSaveImage
  (_fun (filename image (params #f)) ::
        (filename : _file)
        (image : _pointer)
        (params : _pointer)
        -> (r : _int)
        -> (check-return r 'cvSaveImage)))

(define+provide (imwrite filename img (params #f))
  (cvSaveImage filename img params))

;; decode image stored in the buffer
(define-opencv-highgui cvDecodeImage
  (_fun _pointer _int -> _pointer))

(define-opencv-highgui cvDecodeImageM
  (_fun _pointer _int -> _pointer))

;; encode image and store the result as a byte vector (single-row 8uC1 matrix)
(define-opencv-highgui cvEncodeImage
  (_fun _string _pointer _pointer -> _pointer))

(define+provide CV_CVTIMG_FLIP      1)
(define+provide CV_CVTIMG_SWAP_RB   2)

;; utility function: convert one image to another with optional vertical flip
(define-opencv-highgui cvConvertImage
  (_fun _pointer _pointer _int -> _void))

;; (define+provide (convertImage an-image ()
;;   (define+provide out-image (cvCreateImage
;;                      (make-CvSize (IplImage-width an-image)
;;                                   (IplImage-height an-image))
;;                      depth
;;                      (IplImage-nChannels an-image)))
;;   (cvConvertImage an-image out-image depth)
;;   out-image)

;; wait for key event infinitely (delay<=0) or for "delay" milliseconds
(define-opencv-highgui cvWaitKey
  (_fun _int -> _int))


;; ********************************************************************
;;                       Obsolete functions/synonyms
;; ********************************************************************
(define+provide cvCaptureFromFile cvCreateFileCapture)
(define+provide cvCaptureFromCAM cvCreateCameraCapture)
(define+provide cvCaptureFromAVI cvCaptureFromFile)  
(define+provide cvCreateAVIWriter cvCreateVideoWriter)
(define+provide cvWriteToAVI cvWriteFrame)
(define+provide (cvAddSearchPath path) (void))
(define+provide cvvInitSystem cvInitSystem)
(define+provide cvvNamedWindow cvNamedWindow)
(define+provide cvvShowImage cvShowImage)
(define+provide cvvResizeWindow cvResizeWindow)
(define+provide cvvDestroyWindow cvDestroyWindow)
(define+provide cvvCreateTrackbar cvCreateTrackbar)
(define+provide (cvvLoadImage name) (cvLoadImage name 1))
(define+provide cvvSaveImage cvSaveImage)
(define+provide cvvAddSearchPath cvAddSearchPath)
(define+provide (cvvWaitKey name) (cvWaitKey 0))
(define+provide (cvvWaitKeyEx name delay) (cvWaitKey delay))
(define+provide cvvConvertImage cvConvertImage)
(define+provide HG_AUTOSIZE CV_WINDOW_AUTOSIZE)
