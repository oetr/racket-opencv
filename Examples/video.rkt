#!/Users/petr/Applications/Racket/bin/racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; Capture video from a camera
;; if some key is pressed while the focus is on the video window, the application terminates
;; Tested with iSight camera of my MacBook Pro

;;; Includes
(require ffi/unsafe
         ffi/unsafe/define)

(require "../types.rkt")
(require "../highgui.rkt")
(require "../core.rkt")
(require "../imgproc.rkt")

(define image2 #f)
(define a (cvCaptureFromCAM 0))

(let loop ()
  (cvGrabFrame a)
  (set! image2 (ptr-ref (cvRetrieveFrame a 0) _IplImage))
  (cvDilate image2 image2 #f 10)
  (cvShowImage "Video Capture" image2)
  (define key (cvWaitKey 1))
  (unless (>= key 0)
    (loop)))

(cvDestroyWindow "Video Capture")