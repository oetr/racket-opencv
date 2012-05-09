#! /usr/bin/env racket
#lang racket

;; Author: Petr Samarin
;; Date: 2012
;; Description:
;; Load an image and invert all pixel values
;; Original example converted from http://www.cs.iit.edu/~agam/cs512/lect-notes/opencv-intro/

;;; Includes
(require ffi/unsafe
         ffi/unsafe/define)

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

;;; Get image data
(define data
  (make-sized-byte-string (IplImage-imageData img)
                          (* width height channels)))

(time (let loop ([i (- (* width height channels) 1)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (bytes-set! data i (- 255 (bytes-ref data i)))
          (loop (- i 1)))))

;;(cvSetImageROI img (make-CvRect 100 100 200 200))

(define (make-c-array size type)
  (define a (_array type size))
  (define ptr (malloc type 'atomic))
  (ptr-ref ptr a))

(define (make-c-union type)
  (define a (_union (_cpointer _ubyte) (_cpointer _float)))
  (define ptr (malloc type 'atomic))
  (ptr-ref ptr a))

;; (define cvscalar (make-c-array 4 _double))
;; (array-set! cvscalar 0 -100.0)
;; (array-set! cvscalar 1 100.0)
;; (array-set! cvscalar 2 0.0)
;; (array-set! cvscalar 3 0.0)

(time (cvErode img img #f 5))

(printf "adds time: ~n")
(time (cvAddS img (make-CvScalar cvscalar) img #f))
(cvSetImageROI img (make-CvRect 0 0 width height))

(define n-channel-matrix
  (_array _ubyte (* width height channels)))


(define array-data (ptr-ref (IplImage-imageData img)
                            n-channel-matrix))

(time (let loop ([i (- (* width height channels) 1)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (array-set! array-data i (- 255 (array-ref array-data i)))
          (loop (- i 1)))))

(define N (* height width channels))
(define a-vector (make-vector (* width height channels) 0))

(time (let loop ([i (- (* 640 480 3) 1)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (vector-set! a-vector i (- 255 (vector-ref a-vector i)))
          (loop (- i 1)))))

;;; Invert all pixels using pointers: --> very slow
;; TODO: provide a C function to speed this up?
(time (let loop ([i (- (* width height channels) 1)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (ptr-set! data _ubyte i (- 255 (ptr-ref data _ubyte i)))
          (loop (- i 1)))))

(time (let loop ([i (- (* width height channels) 1)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (ptr-set! data _byte i (- 255 (ptr-ref data _byte i)))
          (loop (- i 1)))))

;;; Show the image
(cvNamedWindow "Main Window" CV_WINDOW_AUTOSIZE)
(cvShowImage "Main Window" img)

;;; Wait for a key before destroying the window
(cvWaitKey 0)

;;; Destroy image window
(cvDestroyWindow "Main Window")
