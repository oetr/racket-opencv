#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; Load an image and invert all pixel values
;; Original example converted from http://www.cs.iit.edu/~agam/cs512/lect-notes/opencv-intro/

;;; Includes
(require ffi/unsafe)

(require "../types.rkt")
(require "../highgui.rkt")
(require "../core.rkt")
(require "../imgproc.rkt")

;;; Load an image from the hard disk
(define img (cvLoadImage "images/test.png" CV_LOAD_IMAGE_COLOR))

;;; Get image properties
(define height     (IplImage-height img))
(define width      (IplImage-width img))
(define step       (- (IplImage-widthStep img) 1))
(define channels   (IplImage-nChannels img))

(printf "width: ~a, height: ~a, step: ~a, channels: ~a~n"
        width height step channels)

;;; Image processing
;; set image's region of interest to rectangle (100, 100) -> (200, 200)
(cvSetImageROI img (make-CvRect 100 100 200 200))

;; erode the ROI in-place 1 time for each pixel
(cvErode img img #f 1)

;; set back the ROI
(cvSetImageROI img (make-CvRect 0 0 width height))

;;; IplImage manipulation using Racket bytes
;; convert data to bytestring
(define data
  (make-sized-byte-string (IplImage-imageData img)
                          (* width height channels)))
;; TODO: make this conversion to Racket bytes automatically
;; without the need to require ffi/unsafe library

;; Invert all pixel values
;; Slow image manipulation by using bytes
(let loop ([i (- (* width height channels) 1)])
  (when (>= i 0)
    ;; invert each pixel channel-wise
    (bytes-set! data i (- 255 (bytes-ref data i)))
    (loop (- i 1))))

;;; Show the image
(cvNamedWindow "Main Window" CV_WINDOW_AUTOSIZE)
(cvShowImage "Main Window" img)

;;; Wait for a key before destroying the window
(define key (cvWaitKey 0))
(printf "received key: ~a~n" key)

;;; Destroy image window
(cvDestroyWindow "Main Window")
