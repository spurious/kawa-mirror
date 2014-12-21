;; Test for Savannah bug #43750 "srfi-2 doesn't work inside r7rs library".
(define-library (foo)
  (export foo)
  (import (scheme base) (srfi 2))
  (begin
    (define (foo)
      (and-let* () 1))))

(import (scheme base)
        (scheme write)
        (foo))
(format #t "foo:~w~%" (foo))
;; Output: foo:1
