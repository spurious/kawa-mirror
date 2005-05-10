;; This module should be compiled with --full-tailcalls.
;(module-static #t)

;; Bug reported 2005-05-08 by dominique.boucher@nuecho.com.
(define (neg-abs x)
  (if (< x 0)
      x
      (- x)))
