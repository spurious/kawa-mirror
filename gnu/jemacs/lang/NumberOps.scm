(define (\1- x) (- x 1))

(define (\1+ x) (+ x 1))

(define (% x y)
  (invoke-static <integer> 'remainder x y))
