#|
utilities.rkt: helpers to reduce complexity
Copyleft (C) Peter Samarin
|#
(module opencv-lib racket
  (require ffi/unsafe)
           
  (provide check-return)
  (define (check-return value who)
    (when (<= value 0)
      (error who "bad return value: ~a" value)))

   (provide var->ptr)
   (define-syntax var->ptr
     (syntax-rules ()
       [(var->ptr type val)
        (let ([var (malloc 'atomic type 1)])
          (ptr-set! var type val)
          var)]))

  )
