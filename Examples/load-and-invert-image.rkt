;;; Description
;; Author: Petr Samarin
;; Date: 2012
;; Original example converted from http://www.cs.iit.edu/~agam/cs512/lect-notes/opencv-intro/

(require ffi/unsafe
         ffi/unsafe/define)

(require "../types.rkt")
(require "../highgui.rkt")

;; Create an image structure and load it from the hard disk
(define img
  (ptr-ref
   (cvLoadImage "test.png" CV_LOAD_IMAGE_GRAYSCALE) 
   _IplImage))


;; get the image data
(define height     (IplImage-height img))
(define width      (IplImage-width img))
(define step       (IplImage-widthStep img))
(define channels   (IplImage-nChannels img))

(printf "width: ~a, height: ~a, step: ~a, channels: ~a~n"
        width height step channels)

(define (make-c-array size type)
  (define a (_array type size))
  (define ptr (malloc type 'atomic))
  (ptr-ref ptr a))


(define n-channel-matrix
  (_array (_array _ubyte (* width channels)) height))

(define data (ptr-ref (IplImage-imageData img)
                      n-channel-matrix))

(for* ([y (in-range 0 height)]
       [x (in-range 0 width)])
      (array-set! data y x (- 255 (array-ref data y x))))

;; Show the image
(cvNamedWindow "Main Window" CV_WINDOW_AUTOSIZE)
(cvShowImage "Main Window" img)



;; Destroy image window
(cvDestroyWindow "Main Window")

