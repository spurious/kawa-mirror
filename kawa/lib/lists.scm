(define (pair? x)
  (instance? x <pair>))

(define (null? x)
  (eq? x '()))

(define (set-car! (p <pair>) x)
  ((primitive-set-field <pair> "car" <object>)
   p x))

(define (set-cdr! (p <pair>) x)
  ((primitive-set-field <pair> "cdr" <object>)
   p x))

#|
(define (length list)
  ((primitive-static-method "kawa.lang.List" "length" "int" ())
   list))
|#
