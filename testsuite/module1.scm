(module-export list-length-1 list-length-3)

(define (list-length-1 x) :: <integer> (length x))
(define (list-length-2 x) :: <int> (list-length-1 x))
(define list-length-3 #t)
(set! list-length-3 list-length-2)
