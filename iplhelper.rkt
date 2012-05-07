(module highgui racket
  (provide (all-defined-out))
;;; Libraries
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define)
  
  (define-ffi-definer define-iplhelper
    (ffi-lib "../C-helper-functions/IplImage/liblibraryipl"))
  
  ;; Ported OpenCV requirements
  (require "../types.rkt")
  (require "../highgui.rkt")

  (define-iplhelper ipl_data_ref
    (_fun _pointer _int -> _int))

  (define-iplhelper ipl_data_set
    (_fun _pointer _int _ubyte -> _void))

  (define-iplhelper invert_ipl_matrix
    (_fun _pointer _int -> _void))

  (define-iplhelper ipl_apply_fn
    (_fun _pointer _pointer _int -> _void))

  (define (invert-pixel-value num)
    (function-ptr
     (lambda (x) (- 255 num))
     (_fun (num) :: (num : _int) -> _int)))

  (define t (get-ffi-obj "ipl_apply_fn"
               (ffi-lib "../C-helper-functions/IplImage/liblibraryipl")
               (_cprocedure
                (list (_cprocedure (list _int)
                                   _int)
                      _pointer
                      _int)
                _void)))
  
  (time* (t (lambda (x) (- 255 x)) data (* width height channels)))
  
  (cvShowImage "Main Window" img)

  
   

  (ipl_apply_fn invert-pixel-value data (* width height channels))
  
  (define img
  (ptr-ref
   (cvLoadImage "images/test-image.png" CV_LOAD_IMAGE_COLOR)
   _IplImage))

  (define height     (IplImage-height img))
  (define width      (IplImage-width img))
  (define step       (- (IplImage-widthStep img) 1))
  (define channels   (IplImage-nChannels img))>

  (printf "width: ~a, height: ~a, step: ~a, channels: ~a~n"
          width height step channels)

  (define data (IplImage-imageData img))
  (ipl_data_ref data 0)

  (time (let loop ([i (* width height channels)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (- 255 (ptr-ref data _ubyte i))
          (loop (- i 1)))))

  (time (let loop ([i (* width height channels)])
        (when (>= i 0)
          ;; invert each pixel channel-wise
          (ipl_data_set data i (- 255 (ipl_data_ref data i)))
          (loop (- i 1)))))
  
  (time (invert_ipl_matrix data (* width height channels)))
  (cvShowImage "Main Window" img)
  
  )