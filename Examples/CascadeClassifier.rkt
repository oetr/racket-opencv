#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description: Cascade classifier
;; ported from an OpenCV tutorial http://docs.opencv.org/doc/tutorials/objdetect/cascade_classifier/cascade_classifier.html

(require "../src/types.rkt"
         "../src/highgui.rkt"
         "../src/core.rkt"
         "../src/imgproc.rkt"
         "../src/objdetect.rkt"
         ffi/unsafe)

;; Global variables
(define face-cascade-name "haarcascade_frontalface_alt.xml")
(define eyes-cascade-name "haarcascade_eye_tree_eyeglasses.xml")
(define window-name "Capture - Face detection")
;; load classifiers
(define face-cascade (cvLoadHaarClassifierCascade face-cascade-name
                                                  (make-CvSize 24 24)))
(define eyes-cascade (cvLoadHaarClassifierCascade eyes-cascade-name
                                                  (make-CvSize 24 24)))

(define capture (cvCaptureFromCAM 0))
(define param-set #f)
(set! param-set (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_WIDTH 640.0))
(set! param-set (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_HEIGHT 480.0))
(define frame (cvQueryFrame capture))
;; Get parameters from the captured image to initialize
;; copied images
(define width    (IplImage-width frame))
(define height   (IplImage-height frame))
(define size     (make-CvSize width height))
(define depth    (IplImage-depth frame))
(define channels (IplImage-nChannels frame))

(define (detect-and-display frame)
  (define frame-gray (cvCreateImage size IPL_DEPTH_8U 1))
  (cvConvertImage frame frame-gray IPL_DEPTH_8U)
  (cvEqualizeHist frame-gray frame-gray)
  (define mem (cvCreateMemStorage))
  (define seq (cvHaarDetectObjects frame-gray face-cascade mem 1.1 2
                                   (bitwise-ior 0 CV_HAAR_SCALE_IMAGE)
                                   (make-CvSize 10 10)))

  (printf "found ~a faces~n" (CvSeq-total seq))
  (define f #f)
  (when (> (CvSeq-total seq) 0)
    (set! f (ptr-ref (CvSeq-first seq) _CvSeqBlock)))
  (for ([i (in-range (CvSeq-total seq))])
       (define data (ptr-ref (CvSeqBlock-data f) _CvRect))
       (define x (CvRect-x data))
       (define y (CvRect-y data))
       (define w (CvRect-width data))
       (define h (CvRect-height data))
       (printf "found rect: ~a, ~a, ~a, ~a~n"x y w h)
       
       (cvRectangle frame
                    (cvPoint x y)
                    (cvPoint (+ x w) (+ y h))
                    (cvRGB 0 255 0))
       (define next (CvSeqBlock-next f))
       (when next
         (set! f (ptr-ref next _CvSeqBlock))))
  (imshow window-name frame))


(let loop ()
  (set! frame (cvQueryFrame capture))
  (detect-and-display frame)
  ;;(cvReleaseImage out)  
  (unless (>= (cvWaitKey 1) 0)
    (loop)))

(cvReleaseCapture capture)
(cvDestroyWindow window-name)
