(define-library (evn)
  (import (scheme base) (odd))
  (begin
    (define (is-even? (x :: <int>)) :: <boolean>
      (if (= x 0) #t (is-odd? (- x 1)))))
  (export is-even?))  

(define-library (odd)
  (import (scheme base) (evn))
  ;; Not allowed in pedantic r7rs
  (define (is-odd? (x :: <int>)) :: <boolean>
    (if (= x 0) #f (is-even? (- x 1))))
  (export is-odd?))

(import (odd))
(format #t "odd(21): ~w~%" (is-odd? 21))

;; Output: odd(21): #t
