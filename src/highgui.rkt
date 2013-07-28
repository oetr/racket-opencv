;; Author: Peter Samarin

(module highgui racket 
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/winapi
           ;; OpenCV requirements
           "types.rkt"
           "core.rkt"
           "utilities.rkt")

  (define stype (system-type))
  
  (define opencv-highgui-lib
    (case stype
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
  
  ;; display image within window (highgui windows remember their content)
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

  ;; *********************************************************************************
  ;;                        Working with Video Files and Cameras
  ;; *********************************************************************************
  ;; "black box" capture structure */  
  (define+provide _CvCapture _pointer)

  ;; start capturing frames from video file
  (define-opencv-highgui cvCreateFileCapture
    (_fun _string -> _pointer))

  (define+provide CV_CAP_ANY      0)     ;; autodetect

  (define+provide CV_CAP_MIL      100)   ;; MIL proprietary drivers

  (define+provide CV_CAP_VFW      200)   ;; platform native
  (define+provide CV_CAP_V4L      200)
  (define+provide CV_CAP_V4L2     200)

  (define+provide CV_CAP_FIREWARE 300)   ;; IEEE 1394 drivers
  (define+provide CV_CAP_FIREWIRE 300)
  (define+provide CV_CAP_IEEE1394 300)
  (define+provide CV_CAP_DC1394   300)
  (define+provide CV_CAP_CMU1394  300)

  (define+provide CV_CAP_STEREO   400)   ;; TYZX proprietary drivers
  (define+provide CV_CAP_TYZX     400)
  (define+provide CV_TYZX_LEFT    400)
  (define+provide CV_TYZX_RIGHT   401)
  (define+provide CV_TYZX_COLOR   402)
  (define+provide CV_TYZX_Z       403)

  (define+provide CV_CAP_QT       500)   ;; QuickTime

  (define+provide CV_CAP_UNICAP   600)   ;; Unicap drivers

  (define+provide CV_CAP_DSHOW    700)   ;; DirectShow (via videoInput)

  (define+provide CV_CAP_PVAPI    800)   ;; PvAPI, Prosilica GigE SDK

  (define+provide CV_CAP_OPENNI   900)   ;; OpenNI (for Kinect)

  (define+provide CV_CAP_ANDROID  1000)  ;; Android
  
  (define+provide CV_CAP_XIAPI    1100)   ;; XIMEA Camera API

  ;; start capturing frames from camera:
  ;; index = camera_index + domain_offset (CV_CAP_*)
  (define-opencv-highgui cvCreateCameraCapture
    (_fun _int -> _pointer))

  ;; grab a frame, return 1 on success, 0 on fail.
  ;; this function is thought to be fast
  (define-opencv-highgui cvGrabFrame
    (_fun _pointer
          -> (r : _int)
          -> (check-return r 'cvGrabFrame)))

  ;; get the frame grabbed with cvGrabFrame(..)
  ;; This function may apply some frame processing like
  ;; frame decompression, flipping etc.
  ;; !!!DO NOT RELEASE or MODIFY the retrieved frame!!!
  (define-opencv-highgui cvRetrieveFrame
    (_fun _pointer _int
          -> (image : _pointer)
          -> (ptr-ref image _IplImage)))
  
  ;; just a combination of cvGrabFrame and cvRetrieveFrame
  ;; !!!DO NOT RELEASE or MODIFY the retrieved frame!!!
  (define-opencv-highgui cvQueryFrame
    (_fun _pointer
          -> (r : _pointer)
          -> (ptr-ref r _IplImage)))

  ;; stop capturing/reading and free resources
  (define-opencv-highgui cvReleaseCapture
    (_fun (_ptr i _pointer) -> _void))

  (define+provide (cv-query-frame capture)
    (define image (cvQueryFrame capture))
    (define mat (cvCreateMat (IplImage-height image)
                             (IplImage-width image)
                             (CV_MAKETYPE (IplImage-depth image)
                                          (IplImage-nChannels image))))
    (cvConvert image mat)
    (cvReleaseImage image)
    mat)

  ;; *************************************************************
  ;; modes of the controlling registers (can be: auto, manual,
  ;; auto single push, absolute Latter allowed with any other mode)
  ;; every feature can have only one mode turned on at a time
  ;; *************************************************************
  ;;turn the feature off (not controlled manually nor automatically)
  (define+provide CV_CAP_PROP_DC1394_OFF -4)  
  ;;set automatically when a value of the feature is set by the user
  (define+provide CV_CAP_PROP_DC1394_MODE_MANUAL -3) 
  (define+provide CV_CAP_PROP_DC1394_MODE_AUTO    -2)
  (define+provide CV_CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO  -1)
  (define+provide CV_CAP_PROP_POS_MSEC       0)
  (define+provide CV_CAP_PROP_POS_FRAMES     1)
  (define+provide CV_CAP_PROP_POS_AVI_RATIO  2)
  (define+provide CV_CAP_PROP_FRAME_WIDTH    3)
  (define+provide CV_CAP_PROP_FRAME_HEIGHT   4)
  (define+provide CV_CAP_PROP_FPS            5)
  (define+provide CV_CAP_PROP_FOURCC         6)
  (define+provide CV_CAP_PROP_FRAME_COUNT    7)
  (define+provide CV_CAP_PROP_FORMAT         8)
  (define+provide CV_CAP_PROP_MODE           9)
  (define+provide CV_CAP_PROP_BRIGHTNESS    10)
  (define+provide CV_CAP_PROP_CONTRAST      11)
  (define+provide CV_CAP_PROP_SATURATION    12)
  (define+provide CV_CAP_PROP_HUE           13)
  (define+provide CV_CAP_PROP_GAIN          14)
  (define+provide CV_CAP_PROP_EXPOSURE      15)
  (define+provide CV_CAP_PROP_CONVERT_RGB   16)
  (define+provide CV_CAP_PROP_WHITE_BALANCE_BLUE_U 17)
  (define+provide CV_CAP_PROP_RECTIFICATION 18)
  (define+provide CV_CAP_PROP_MONOCROME     19)
  (define+provide CV_CAP_PROP_SHARPNESS     20)
  ;; exposure control done by camera
  ;; user can adjust reference level using this feature
  (define+provide CV_CAP_PROP_AUTO_EXPOSURE 21)
  (define+provide CV_CAP_PROP_GAMMA         22)
  (define+provide CV_CAP_PROP_TEMPERATURE   23)
  (define+provide CV_CAP_PROP_TRIGGER       24)
  (define+provide CV_CAP_PROP_TRIGGER_DELAY 25)
  (define+provide CV_CAP_PROP_WHITE_BALANCE_RED_V 26)
  (define+provide CV_CAP_PROP_MAX_DC1394    27)
  ;; property for highgui class
  ;; CvCapture_Android only
  (define+provide CV_CAP_PROP_AUTOGRAB      1024)
  ;; readonly, tricky
  ;; property, returns cpnst char* indeed
  (define+provide CV_CAP_PROP_SUPPORTED_PREVIEW_SIZES_STRING 1025)
  ;; readonly, tricky property, returns const char* indeed
  (define+provide CV_CAP_PROP_PREVIEW_FORMAT 1026) 
  
  ;; OpenNI map generators
  (define+provide CV_CAP_OPENNI_DEPTH_GENERATOR  0)
  (define+provide CV_CAP_OPENNI_IMAGE_GENERATOR  (- (expt 2 31)))
  (define+provide CV_CAP_OPENNI_GENERATORS_MASK  (- (expt 2 31)))

  ;; Properties of cameras available through OpenNI interfaces
  (define+provide CV_CAP_PROP_OPENNI_OUTPUT_MODE       100)
  (define+provide CV_CAP_PROP_OPENNI_FRAME_MAX_DEPTH   101) ;; in mm
  (define+provide CV_CAP_PROP_OPENNI_BASELINE          102) ;; in mm
  (define+provide CV_CAP_PROP_OPENNI_FOCAL_LENGTH      103) ;; in pixels
  (define+provide CV_CAP_PROP_OPENNI_REGISTRATION_ON   104) ;; flag
  ;; flag that synchronizes the remapping depth map to image map
  ;; by changing depth generator's view point (if the flag is "on") or
  ;; sets this view point to its normal one (if the flag is "off").
  (define+provide CV_CAP_PROP_OPENNI_REGISTRATION      CV_CAP_PROP_OPENNI_REGISTRATION_ON) 
  (define+provide CV_CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE
    (+ CV_CAP_OPENNI_IMAGE_GENERATOR CV_CAP_PROP_OPENNI_OUTPUT_MODE))
  (define+provide CV_CAP_OPENNI_DEPTH_GENERATOR_BASELINE
    (+ CV_CAP_OPENNI_DEPTH_GENERATOR CV_CAP_PROP_OPENNI_BASELINE))
  (define+provide CV_CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH
    (+ CV_CAP_OPENNI_DEPTH_GENERATOR CV_CAP_PROP_OPENNI_FOCAL_LENGTH))
  (define+provide CV_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON
    (+ CV_CAP_OPENNI_DEPTH_GENERATOR CV_CAP_PROP_OPENNI_REGISTRATION_ON))
  
  ;; Properties of cameras available through GStreamer interface
  (define+provide CV_CAP_GSTREAMER_QUEUE_LENGTH    200) ;; default is 1
  ;; ip for anable multicast master mode. 0 for disable multicast
  (define+provide CV_CAP_PROP_PVAPI_MULTICASTIP    300) 
  
  ;; Properties of cameras available through XIMEA SDK interface
  ;; Change image resolution by binning or skipping.  
  (define+provide CV_CAP_PROP_XI_DOWNSAMPLING   400)
  (define+provide CV_CAP_PROP_XI_DATA_FORMAT    401)      ;; Output data format.
  ;; Horizontal offset from the origin to the area of interest (in pixels).
  (define+provide CV_CAP_PROP_XI_OFFSET_X       402)
  ;; Vertical offset from the origin to the area of interest (in pixels).
  (define+provide CV_CAP_PROP_XI_OFFSET_Y       403)
  (define+provide CV_CAP_PROP_XI_TRG_SOURCE     404)      ;; Defines source of trigger.
  ;; Generates an internal trigger. PRM_TRG_SOURCE must be set to TRG_SOFTWARE.
  (define+provide CV_CAP_PROP_XI_TRG_SOFTWARE   405)      
  (define+provide CV_CAP_PROP_XI_GPI_SELECTOR   406)      ;; Selects general purpose input 
  (define+provide CV_CAP_PROP_XI_GPI_MODE       407)      ;; Set general purpose input mode
  (define+provide CV_CAP_PROP_XI_GPI_LEVEL      408)      ;; Get general purpose level
  (define+provide CV_CAP_PROP_XI_GPO_SELECTOR   409)      ;; Selects general purpose output 
  (define+provide CV_CAP_PROP_XI_GPO_MODE       410)      ;; Set general purpose output mode
  (define+provide CV_CAP_PROP_XI_LED_SELECTOR   411)      ;; Selects camera signalling LED
  ;; Define+Provide camera signalling LED functionality
  (define+provide CV_CAP_PROP_XI_LED_MODE       412)
  ;; Calculates White Balance(must be called during acquisition)
  (define+provide CV_CAP_PROP_XI_MANUAL_WB      413)      
  (define+provide CV_CAP_PROP_XI_AUTO_WB        414)      ;; Automatic white balance
  (define+provide CV_CAP_PROP_XI_AEAG           415)      ;; Automatic exposure/gain
  ;; Exposure priority (0.5 - exposure 50%, gain 50%).
  (define+provide CV_CAP_PROP_XI_EXP_PRIORITY   416)
  ;; Maximum limit of exposure in AEAG procedure
  (define+provide CV_CAP_PROP_XI_AE_MAX_LIMIT   417)
  ;; Maximum limit of gain in AEAG procedure
  (define+provide CV_CAP_PROP_XI_AG_MAX_LIMIT   418)
  ;; Average intensity of output signal AEAG should achieve(in %)
  (define+provide CV_CAP_PROP_XI_AEAG_LEVEL     419)
  ;; Image capture timeout in milliseconds
  (define+provide CV_CAP_PROP_XI_TIMEOUT        420)


  ;; Data given from depth generator.
  (define+provide CV_CAP_OPENNI_DEPTH_MAP         0) ;; Depth values in mm (CV_16UC1)
  (define+provide CV_CAP_OPENNI_POINT_CLOUD_MAP   1) ;; XYZ in meters (CV_32FC3)
  (define+provide CV_CAP_OPENNI_DISPARITY_MAP     2) ;; Disparity in pixels (CV_8UC1)
  (define+provide CV_CAP_OPENNI_DISPARITY_MAP_32F 3) ;; Disparity in pixels (CV_32FC1)
  (define+provide CV_CAP_OPENNI_VALID_DEPTH_MASK  4) ;; (CV_8UC1)

  ;; Data given from RGB image generator.
  (define+provide CV_CAP_OPENNI_BGR_IMAGE         5)
  (define+provide CV_CAP_OPENNI_GRAY_IMAGE        6)


  ;; Supported output modes of OpenNI image generator
  (define+provide CV_CAP_OPENNI_VGA_30HZ      0)
  (define+provide CV_CAP_OPENNI_SXGA_15HZ     1)


  ;; supported by Android camera output formats
  (define+provide CV_CAP_ANDROID_COLOR_FRAME_BGR  0) ;; BGR
  (define+provide CV_CAP_ANDROID_COLOR_FRAME  CV_CAP_ANDROID_COLOR_FRAME_BGR)
  (define+provide CV_CAP_ANDROID_GREY_FRAME   1)  ;; Y
  (define+provide CV_CAP_ANDROID_COLOR_FRAME_RGB  2)
  (define+provide CV_CAP_ANDROID_COLOR_FRAME_BGRA  3)
  (define+provide CV_CAP_ANDROID_COLOR_FRAME_RGBA  4)

  ;; retrieve or set capture properties
  (define-opencv-highgui cvGetCaptureProperty
    (_fun _pointer _int -> _double))
  
  (define-opencv-highgui cvSetCaptureProperty
    (_fun _pointer _int _double
          -> (r : _int)
          -> (check-return r 'cvSetCaptureProperty)))

  ;; Return the type of the capturer (eg, CV_CAP_V4W, CV_CAP_UNICAP),
  ;; which is unknown if created with CV_CAP_ANY
  (define-opencv-highgui cvGetCaptureDomain
    (_fun _pointer -> _int))

  ;; "black box" video file writer structure (not used in Racket)
  (define+provide CvVideoWriter _pointer)

  ;; CV_FOURCC: symbol symbol symbol symbol -> integer
  ;; combines the symbols to produce a number
  ;; each symbol is converted to ASCII and shifted to one of 4 bands
  ;; first band is between 0 and 8 bits, second 8 to 16 bits, etc
  ;; on this way, it is possible to uniquely map 4 chars to a number
  (define+provide (CV_FOURCC c1 c2 c3 c4)
    (let ([c1 (char->integer (string-ref (symbol->string c1) 0))]
          [c2 (char->integer (string-ref (symbol->string c2) 0))]
          [c3 (char->integer (string-ref (symbol->string c3) 0))]
          [c4 (char->integer (string-ref (symbol->string c4) 0))])
      (+ (bitwise-and c1 255)
         (arithmetic-shift (bitwise-and c2 255) 8)
         (arithmetic-shift (bitwise-and c3 255) 16)
         (arithmetic-shift (bitwise-and c4 255) 24))))

  ;; Open Codec Selection Dialog (Windows only)
  (define+provide CV_FOURCC_PROMPT -1)
  
  (define+provide CV_FOURCC_DEFAULT (CV_FOURCC 'I 'Y 'U 'V))

  ;; initialize video file writer
  (define-opencv-highgui cvCreateVideoWriter
    (_fun _file _int _double _CvSize _int
          -> (r : _int)
          -> (check-return r 'cvCreateVideoWriter)))

  ;; write frame to video file
  (define-opencv-highgui cvWriteFrame
    (_fun _pointer (_ptr i _IplImage)
          -> (r : _int)
          -> (check-return r 'cvWriteFrame)))

  ;; close video file writer
  (define-opencv-highgui cvReleaseVideoWriter
    (_fun (_ptr i _pointer) -> _void))

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


  )
