(define (pair? x)
  (instance? x <pair>))

(define (null? x)
  (eq? x '()))

#|
(define (length list)
  ((primitive-static-method "kawa.lang.List" "length" "int" ())
   list))
|#
