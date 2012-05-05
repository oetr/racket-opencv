;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define)


;;; FFI Definers
(define-ffi-definer define-opencv-highgui
  (ffi-lib "/opt/local/lib/libopencv_highgui"))

(define-ffi-definer define-opencv-core
  (ffi-lib "/opt/local/lib/libopencv_core"))

(define (make-c-array size type)
  (define a (_array type size))
  (define ptr (malloc type 'atomic))
  (ptr-ref ptr a))


(define (array-filter fn array min max)
  (if (= min max) empty
      (let ([value (array-ref array min)])        
        (if (fn value)
            (cons value (array-filter fn array (+ min 1) max))
            (array-filter fn array (+ min 1) max)))))

(define a (make-c-array 20 _int))
(array-filter (lambda (x) (< x 10)) a 20)




;;; Structs
(define data (_union (_cpointer _ubyte)))
(define-cstruct _CvMat
  ([type _int]
   [step _int]
   ;; for internal use only
   [refcount _gcpointer]
   [hdr_refcount _int]
   
   
   ))
typedef struct CvMat
{
    int type;
    int step;

    /* for internal use only */
    int* refcount;
    int hdr_refcount;

    union
    {
        uchar* ptr;
        short* s;
        int* i;
        float* fl;
        double* db;
    } data;

#ifdef __cplusplus
    union
    {
        int rows;
        int height;
    };

    union
    {
        int cols;
        int width;
    };
#else
    int rows;
    int cols;
#endif

}
CvMat;

;;; Procedures
(define-opencv-highgui cvCreateMat (_fun _int _int _int -> _pointer))