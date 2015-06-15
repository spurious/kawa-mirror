;; test library with same name as module.
(define-library (libx)
  (import (scheme base))
  (export libx-report)
  (begin (define (libx-report) '(ax in libx))))
