#|
utilities.rkt: helpers to reduce complexity
Copyleft (C) Petr Samarin
|#
(module opencv-lib racket
  (provide check-return)
  (define (check-return value who)
    (when (<= value 0)
      (error who "bad return value: ~a" value)))
  
  )
