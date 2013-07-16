#|
Author: Peter Samarin
utilities.rkt: helpers to reduce complexity
|#

(module opencv-lib racket
  (provide (all-defined-out))
  (require ffi/unsafe)
  
  (define (check-return value who)
    (when (<= value 0)
      (error who "bad return value: ~a" value)))

  (define-syntax var->ptr
    (syntax-rules ()
      [(var->ptr type val)
       (let ([var (malloc 'atomic type 1)])
         (ptr-set! var type val)
         var)]))

  (define-syntax define+provide
    (syntax-rules ()
      [(_ (name args ... . rest) body ...)
       (begin (provide name)
              (define (name args ... . rest)
                body ...))]
      [(_ (name args ...) body ...)
       (begin (provide name)
              (define (name args ...)
                body ...))]
      [(_ name body)
       (begin
         (provide name)
         (define name body))]))

  (define-syntax define+provide-cstruct
    (syntax-rules ()
      [(_ name body)
       (begin
         (provide name)
         (define-cstruct name body))]))

  )
