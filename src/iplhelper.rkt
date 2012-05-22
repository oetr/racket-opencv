(module highgui racket
  (provide (all-defined-out))
;;; Libraries
  ;; Racket Foreign interface
  (require ffi/unsafe
           ffi/unsafe/define)
  
  (define-ffi-definer define-iplhelper
    (ffi-lib "/Users/petr/Dropbox/Programming/Racket/OpenCV/C-helper-functions/IplImage/liblibraryipl"))
  
  ;; Ported OpenCV requirements
  (require "types.rkt")
  (require "highgui.rkt")

  (define-iplhelper ipl_data_ref
    (_fun _pointer _int -> _int))

  (define-iplhelper ipl_data_set
    (_fun _pointer _int _ubyte -> _void))

  (define-iplhelper invert_ipl_matrix
    (_fun _pointer _int -> _void))

  (define-iplhelper ipl_apply_fn
    (_fun _pointer _pointer _int -> _void))

  (define-iplhelper get_I
    (_fun _pointer _int _int -> _ubyte))

  (define-iplhelper bresentham_circle
    (_fun (img x y p) ::
          (img : _pointer)
          (x : _int)
          (y : _int)
          (p : _bytes)
          -> _void))

  (define-iplhelper are_numbers_contiguous
    (_fun (bitmap vals threshold (N 9)) ::
          (bitmap : _bytes)
          (vals : _bytes)
          (len : _int = (bytes-length bitmap))
          (N : _int)
          (threshold : _ubyte)
          -> _int))

  (define-iplhelper corner_score
    (_fun (intensities bright-bm dark-bm threshold) ::
          (intensities : _bytes)
          (bright-bm : _bytes)
          (dark-bm : _bytes)          
          (len : _int = (bytes-length intensities))
          (threshold : _ubyte)
          -> _int))

  (define-iplhelper detect_corners
    (_fun (src dst threshold) ::
          (src : _pointer)
          (dst : _pointer)
          (threshold : _ubyte)
          -> _void))

  ;; (define get-I get_I)

  (define (invert-pixel-value num)
    (function-ptr
     (lambda (x) (- 255 num))
     (_fun (num) :: (num : _int) -> _int)))

  ;; (define t (get-ffi-obj "ipl_apply_fn"
  ;;                        (ffi-lib "/Users/petr/Dropbox/Programming/Racket/OpenCV/C-helper-functions/IplImage/liblibraryipl")
  ;;                        (_fun (_fun _int -> _int) _pointer _int -> _void)))
  
  ;; (time (t (lambda (x) (- 255 x)) data (* width height channels)))
  
  ;; (cvShowImage "Main Window" img)
  ;; (cvDestroyWindow "Main Window")

  
   

  ;; (ipl_apply_fn invert-pixel-value data (* width height channels))
  
  ;; (define img
  ;; (ptr-ref
  ;;  (cvLoadImage "images/test-image.png" CV_LOAD_IMAGE_COLOR)
  ;;  _IplImage))

  ;; (define height     (IplImage-height img))
  ;; (define width      (IplImage-width img))
  ;; (define step       (- (IplImage-widthStep img) 1))
  ;; (define channels   (IplImage-nChannels img))>

  ;; (printf "width: ~a, height: ~a, step: ~a, channels: ~a~n"
  ;;         width height step channels)

  ;; (define data (IplImage-imageData img))
  ;; (ipl_data_ref data 0)

  ;; (time (let loop ([i (* width height channels)])
  ;;       (when (>= i 0)
  ;;         ;; invert each pixel channel-wise
  ;;         (- 255 (ptr-ref data _ubyte i))
  ;;         (loop (- i 1)))))

  ;; (time (let loop ([i (* width height channels)])
  ;;       (when (>= i 0)
  ;;         ;; invert each pixel channel-wise
  ;;         (ipl_data_set data i (- 255 (ipl_data_ref data i)))
  ;;         (loop (- i 1)))))
  
  ;; (time (invert_ipl_matrix data (* width height channels)))
  ;; (cvShowImage "Main Window" img)
  
  )