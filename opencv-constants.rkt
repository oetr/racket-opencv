(module opencv-constants racket
  (provide (all-defined-out))
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
  (define CV_WINDOW_KEEPRATIO     #x00000000))