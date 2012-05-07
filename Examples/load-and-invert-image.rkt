#!/Users/petr/Applications/Racket/bin/racket
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

;;; Load an image from the hard disk
(define img
  (ptr-ref
   (cvLoadImage "images/test-image.png" CV_LOAD_IMAGE_COLOR)
   _IplImage))

;;; Get image properties
(define height     (IplImage-height img))
(define width      (IplImage-width img))
(define step       (- (IplImage-widthStep img) 1))
(define channels   (IplImage-nChannels img))

(printf "width: ~a, height: ~a, step: ~a, channels: ~a~n"
        width height step channels)

;;; Get image data
(define data #f)
(time (set! data (IplImage-imageData img)))

(define n-channel-matrix
  (_array _ubyte (* width height channels)))

(define array-data #f)

(time (set! array-data (ptr-ref (IplImage-imageData img)
                          n-channel-matrix)))

(time (let loop ([i (- (* width height channels) 1)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (array-set! array-data i (- 255 (array-ref array-data i)))
          (loop (- i 1)))))

(require ffi/cvector)
(require ffi/unsafe/cvector)

(define c-vec (make-cvector _ubyte (* width height channels)))
(define cvec-data #f)

(time (set! cvec-data (ptr-ref (IplImage-imageData img)
                               _cvector)))

(time (set! cvec-data (make-cvector* (IplImage-imageData img)
                                     _ubyte (* width height channels))))
(define cvl #f)
(time (set! cvl (cvector-> cvec-data)))

(time (let loop ([i (- (* width height channels) 1)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (cvector-set! cvec-data i (- 255 (cvector-ref cvec-data i)))
          (loop (- i 1)))))

;;; Invert all pixels using pointers: --> very slow
;; TODO: provide a C function to speed this up?
(time (let loop ([i (* width height channels)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (ptr-set! data _ubyte i (- 255 (ptr-ref data _ubyte i)))
          (loop (- i 1)))))

;;; Show the image
(cvNamedWindow "Main Window" CV_WINDOW_AUTOSIZE)
(cvShowImage "Main Window" img)

;;; Wait for a key before destroying the window
(cvWaitKey 0)

;;; Destroy image window
(cvDestroyWindow "Main Window")