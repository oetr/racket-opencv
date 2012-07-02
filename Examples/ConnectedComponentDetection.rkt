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
         ffi/unsafe)

;; (define img-dir "/Users/petr/TestImages/CornerDetection/")
;; (define img-name "DSC06183.jpg")
;; (define dst (cvLoadImage (string-append img-dir img-name)))
;; (define src (cvCreateImage (cvGetSize dst) 8 1))
;; (cvConvertImage dst src IPL_DEPTH_8U)

;; (define storage (cvCreateMemStorage 0))
;; ;;(define contour (malloc _CvSeq 'atomic))
;; (define contour (malloc (_cpointer _CvSeq) 'atomic))

(define (sequence->list a-seq a-type)
  (define (retrieve-block-objects a-block count)
    (define element (ptr-ref (CvSeqBlock-data a-block) a-type count))
    ;;(printf "~a~n" count)
    (if (zero? count)
        (cons element empty)
        (cons element (retrieve-block-objects a-block (- count 1)))))
  (define total (CvSeq-total a-seq))
  (printf "total: ~a" total)
  (define (traverse-sequence-aux total a-block)
    (define next (seqBlock-next a-block))
    (define start-index (CvSeqBlock-start_index a-block))
    (define count (CvSeqBlock-count a-block))
    (define block-elements (retrieve-block-objects a-block count))
    (if (and next (not (zero? total)))
        (cons block-elements 
              (traverse-sequence-aux (- total 1) next))
        (cons block-elements empty)))
  (define first-block (seq-first a-seq))
  (when first-block
    (traverse-sequence-aux total first-block)))

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

(define (block-chain->list a-block a-type)
  (define next-ptr (CvSeqBlock-next a-block))
  (if (and next-ptr (not (ptr-equal? a-block next-ptr)))
      (let ([next-block (ptr-ref next-ptr _CvSeqBlock)])
        (cons (block->list a-block a-type)
              (block-chain->list next-block a-type)))
      (list (block->list a-block a-type))))

;; (define points (map (lambda (a-sequence)
;;                       (map (lambda (a-list)
;;                              (map (lambda (a-point)
;;                                     (list (CvPoint-x a-point) (CvPoint-y a-point)))
;;                                   a-list))
;;                            (block-chain->list (seq-first a-sequence)
;;                                               _CvPoint)))
;;                     sequences))
;;                     (take sequences 15)))

(define capture (cvCaptureFromCAM 0))
(define param-set #f)
(set! param-set (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_WIDTH 640.0))
(set! param-set (cvSetCaptureProperty capture CV_CAP_PROP_FRAME_HEIGHT 480.0))
(define captured-image (cvQueryFrame capture))

;; Get parameters from the captured image to initialize
;; copied images
(define width    (IplImage-width captured-image))
(define height   (IplImage-height captured-image))
(define size     (make-CvSize width height))
(define depth    (IplImage-depth captured-image))
(define channels (IplImage-nChannels captured-image))

;; Init an IplImage to where captured images will be copied
(define img (cvCreateImage size IPL_DEPTH_8U 1))

(define (random+ limit addition)
  (+ (random limit) addition))

(define (draw-contours! lof-sequences img)
  (andmap (lambda (a-sequence)
            (define color (CV_RGB (random+ 155 100)
                                  (random+ 155 100)
                                  (random+ 155 100)))
            (cvDrawContours img a-sequence  color color -1 1)
            (cvClearSeq a-sequence))
          lof-sequences))

(define min-threshold 50.0)

;; Add a trackbar
(define a (malloc 'atomic _int))
(ptr-set! a _int (inexact->exact (floor min-threshold)))
(define (on-trackbar n)
  (set! min-threshold (exact->inexact n))
  ;; slowing down callback function makes the program less likely to crash
  (sleep 0.1))

(cvShowImage "captured" captured-image)
(cvCreateTrackbar "Corner Threshold" "captured" a 255 on-trackbar)

(let loop ()
  (set! captured-image (cvQueryFrame capture))
  (cvConvertImage captured-image img IPL_DEPTH_8U)
  (cvShowImage "bw" img)
  (cvThreshold img img min-threshold 255.0 CV_THRESH_BINARY)
  (cvShowImage "Binary-image" img)  
  ;; allocate memory
  (define storage (cvCreateMemStorage 0))
  (define contour (malloc (_cpointer _CvSeq) 'atomic))
  (cvFindContours img storage contour 128 CV_RETR_EXTERNAL
                  CV_RETR_TREE;; CV_RETR_TREE
                  ;;CV_CHAIN_APPROX_NONE)
                  )
  (when (ptr-ref contour _pointer)
    (define seq (ptr-ref (ptr-ref contour _pointer) _CvSeq))
    (define sequences (sequence-chain->list seq))
    (draw-contours! sequences captured-image))
  (cvShowImage "captured" captured-image)
  (cvReleaseMemStorage storage)
  (unless (>= (cvWaitKey 10) 0)
    (loop)))

;;(cvShowImage "Destination" dst)
;;(cvSaveImage (string->path (string-append img-dir "new-image.jpg") ) dst)
