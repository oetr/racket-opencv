#|
utilities.rkt: helpers to reduce complexity
Copyleft (C) Petr Samarin
|#
(module opencv-lib racket
  (provide check-return)
  (define (check-return value who)
    (unless (zero? value)
      (error who "failed: ~a" value)))
  

  )
