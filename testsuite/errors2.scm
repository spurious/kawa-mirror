(module-compile-options warn-as-error: #t)

(define (bad-throwable1)
  (primitive-throw java.lang.NullPointerException))
;; Diagnostic: errors2.scm:4:20: type class is incompatible with required type java.lang.Throwable
