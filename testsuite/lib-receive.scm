(define-library (blerg)
  (export foo)
  (import (scheme base)
          (scheme write)
          (srfi 8))
  (begin
    (define (foo)
      (receive stuff (values 1 2)
               (write stuff)
               (newline)))))


(import (scheme base)
        (blerg))
(foo)
;; Output: (1 2)
