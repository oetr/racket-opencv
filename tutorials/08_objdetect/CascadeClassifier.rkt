#! /usr/bin/env racket
#lang racket

;; Author: Peter Samarin
;; Date: 2013
;; Description: Cascade classifier
;; ported from an OpenCV tutorial http://docs.opencv.org/doc/tutorials/objdetect/cascade_classifier/cascade_classifier.html

(require (planet petr/opencv/highgui)
         (planet petr/opencv/imgproc)
         (planet petr/opencv/objdetect)
         ffi/unsafe)

;; Global variables
(define face-cascade-name "../data/haarcascade_frontalface_alt.xml")
(define eyes-cascade-name "../data/haarcascade_eye_tree_eyeglasses.xml")
(define window-name "Capture - Face detection")
;; load classifiers
(define face-cascade (cvLoadHaarClassifierCascade face-cascade-name
                                                  (make-CvSize 24 24)))
(define eyes-cascade (cvLoadHaarClassifierCascade eyes-cascade-name
                                                  (make-CvSize 24 24)))

;; open camera and set capture parameters
(define capture (cvCaptureFromCAM 0))
(define param-set #f)
(set! param-set (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_WIDTH 640.0))
(set! param-set (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_HEIGHT 480.0))

;; get one frame to figure out the image parameters
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
                                   (make-CvSize 3 3)))
  

  (define (traverse-sequence a-seq)
    (define total (CvSeq-total a-seq))
    (define (traverse-sequence-aux total a-block)
      (define next (seqBlock-next a-block))
      (define start-index (CvSeqBlock-start_index a-block))
      (define count (CvSeqBlock-count a-block))
      (for ([i (in-range count)])
        (define rect (ptr-ref (CvSeqBlock-data a-block) _CvRect i))
        (define x (CvRect-x rect))
        (define y (CvRect-y rect))
        (define w (CvRect-width rect))
        (define h (CvRect-height rect))
        (cvRectangle frame
                     (cvPoint x y)
                     (cvPoint (+ x w) (+ y h))
                     (cvRGB 0 255 0))
        (cvRectangle frame-gray
                     (cvPoint x y)
                     (cvPoint (+ x w) (+ y h))
                     (cvRGB 0 255 0)))
      (when (and next (not (zero? total)))
        (traverse-sequence-aux (- total 1) next)))
    (define first-block (seq-first a-seq))
    (when first-block
      (traverse-sequence-aux total first-block)))

  (traverse-sequence seq)
  (imshow window-name frame)
  (imshow "gray" frame-gray)

  ;; release unused memory before loosing the pointers
  (cvReleaseImage frame-gray)
  (cvReleaseMemStorage mem))


(let loop ()
  (set! frame (cvQueryFrame capture))
  (detect-and-display frame)
  (unless (>= (cvWaitKey 5) 0)
    (loop)))

(cvReleaseCapture capture)
(cvDestroyWindow window-name)
