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
   (cvLoadImage "test.png" CV_LOAD_IMAGE_GRAYSCALE) 
   _IplImage))

;;; Get image properties
(define height     (IplImage-height img))
(define width      (IplImage-width img))
(define step       (IplImage-widthStep img))
(define channels   (IplImage-nChannels img))

(printf "width: ~a, height: ~a, step: ~a, channels: ~a~n"
        width height step channels)

;;; Get image data
(define (make-c-array size type)
  (define a (_array type size))
  (define ptr (malloc type 'atomic))
  (ptr-ref ptr a))


(define n-channel-matrix
  (_array (_array _ubyte (* width channels)) height))

(define data (ptr-ref (IplImage-imageData img)
                      n-channel-matrix))

;;; Invert all pixels
(for* ([y (in-range 0 height)]
       [x (in-range 0 width)])
      (array-set! data y x (- 255 (array-ref data y x))))

;;; Show the image
(cvNamedWindow "Main Window" CV_WINDOW_AUTOSIZE)
(cvShowImage "Main Window" img)

;;; Wait for a key before destroying the window
(cvWaitKey 0)

;;; Destroy image window
(cvDestroyWindow "Main Window")