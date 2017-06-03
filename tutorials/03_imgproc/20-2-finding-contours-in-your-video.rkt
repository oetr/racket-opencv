#! /usr/bin/env racket
;; Author: Peter Samarin
;; Date: 2013
;; Description: Contours detection in a video stream
;; inspired by http://docs.opencv.org/doc/tutorials/imgproc/shapedescriptors/find_contours/find_contours.html#find-contours
#lang racket

(require ffi/unsafe
         opencv/core
         opencv/types
         opencv/imgproc
         opencv/videoio
         opencv/highgui)

(define (sequence-chain->list a-seq)
  (define next-ptr (CvSeq-h_next a-seq))  
  (if next-ptr
      (let ([next-sequence (ptr-ref next-ptr _CvSeq)])
        (cons a-seq (sequence-chain->list next-sequence)))
      (cons a-seq empty)))

;; seq: get block
;; get all elements from the block
(define (block->list a-block a-type)
  (define data (CvSeqBlock-data a-block))
  (let loop ([count (- (CvSeqBlock-count a-block) 1)])
    (define element (ptr-ref (CvSeqBlock-data a-block) a-type count))
    (if (zero? count)
        (cons element empty)
        (cons element (loop (- count 1))))))

(define (sequence->list a-sequence a-type)
  (define (block-chain->list a-block a-type count)
    (define next-ptr (CvSeqBlock-next a-block))
    (if (and next-ptr (> count 0))
        (let ([next-block (ptr-ref next-ptr _CvSeqBlock)])
          (cons (block->list a-block a-type)
                (block-chain->list next-block a-type
                                   (- count (CvSeqBlock-count next-block)))))
        '()))
  (block-chain->list (seq-first a-sequence) _CvPoint (CvSeq-total a-sequence)))


(define capture (cvCaptureFromCAM 0))

;; Reduce image resolution to 640x480
(cvSetCaptureProperty capture CV_CAP_PROP_FRAME_WIDTH 640.0)
(cvSetCaptureProperty capture CV_CAP_PROP_FRAME_HEIGHT 480.0)

;; Capture an image to get parameters
(define captured-image (cvQueryFrame capture))

;; Get parameters from the captured image to initialize
;; copied images
(define width    (IplImage-width captured-image))
(define height   (IplImage-height captured-image))
(define size     (make-CvSize width height))
(define depth    (IplImage-depth captured-image))
(printf "image width: ~a, image height: ~a~n" width height)

;; Init an IplImage to where captured images will be copied
(define img (cvCreateImage size depth 1))


(define (random+ limit addition)
  (+ (random limit) addition))

(define (draw-contours! lof-sequences img thickness)
  (andmap (lambda (a-sequence)
            (define color (CV_RGB (random+ 155 100)
                                  (random+ 155 100)
                                  (random+ 155 100)))
            (cvDrawContours img a-sequence color color -1 thickness)
            (cvClearSeq a-sequence))
          lof-sequences))

(define min-threshold 5.0)

;; Add a trackbar
(define a (malloc 'atomic _int))
(ptr-set! a _int (inexact->exact (floor min-threshold)))
(define (on-trackbar n)
  (sleep 0.01)
  (set! min-threshold (exact->inexact n)))

(cvShowImage "captured" captured-image)
;;(cvCreateTrackbar "Corner Threshold" "captured" a 255 on-trackbar)
(cvWaitKey 0)

(let loop ()
  (define original-image (cvQueryFrame capture))
  (define captured-image (copy-image original-image))
  (cvCvtColor captured-image img CV_BGR2GRAY)
  (imshow "bw" img)
  (cvWaitKey 1)
  (cvThreshold img img min-threshold 255.0 CV_THRESH_BINARY)
  (imshow "Binary-image" img)
  (cvWaitKey 1)
  
  ;; allocate memory
  (define storage (cvCreateMemStorage 0))
  (define contour (malloc (_cpointer _CvSeq) 'atomic))
  
  (cvFindContours img storage contour 128 CV_RETR_EXTERNAL CV_CHAIN_APPROX_NONE)
  
  (when (ptr-ref contour _pointer)
    (define seq (ptr-ref (ptr-ref contour _pointer) _CvSeq))
    (define sequences (sequence-chain->list seq))
    (draw-contours! sequences captured-image 3))
  (cvShowImage "captured" captured-image)
  (cvWaitKey 1)
  ;; free up the memory
  (cvClearSeq contour)
  (cvReleaseMemStorage storage)
  (unless (>= (cvWaitKey 10) 0)
    (loop)))

(cvDestroyAllWindows)
