(module-static #f)

(define (my-factorial n)
  (if (<= n 1)
      1
      (* n (my-factorial (- n 1)))))

(define-private (list-length-1 (x :: <list>)) :: <double>
   (length x))
(define (list-length-2 x) :: <int>
  (inexact->exact (round (list-length-1 x))))

(require 'list-lib)

(define-syntax deldup
  (syntax-rules ()
   ((deldup list)
    (delete-duplicates list))))

(define (call-to-first x)
  (first x))

(define-namespace date "class:java.util.Date")

(define-syntax namespace-syntax-test
  (syntax-rules ()
    ((namespace-syntax-test)
     (date:parse "6 Sep 2003"))))
