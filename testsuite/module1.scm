(define-private (list-length-1 (x :: <list>)) :: <double>
   (length x))
(define (list-length-2 x) :: <int>
  (inexact->exact (round (list-length-1 x))))

(require 'list-lib)

(define-syntax deldup
  (syntax-rules ()
   ((deldup list)
    (delete-duplicates list))))
