;; Test for Savannah bug #43750 "srfi-2 doesn't work inside r7rs library".
(define-library (foo)
  (export foo1 foo2)
  (import (scheme base) (srfi 2))
  (begin
    (define (foo1)
      (and-let* () 1))
    (define (foo2 path)
      (and-let* ((something #t)) #t))))

(import (scheme base)
        (scheme write)
        (foo))
(format #t "foo1: ~w~%" (foo1))
;; Output: foo1: 1
(format #t "foo2: ~w~%" (foo2 '(1 2 3)))
;; Output: foo2: #t
