(define (pair? x)
  (instance? x <pair>))

(define (cons car cdr)
  (make <pair> car cdr))

(define (null? x)
  (eq? x '()))

(define (set-car! (p <pair>) x)
  ((primitive-set-field <pair> "car" <object>)
   p x))

(define (set-cdr! (p <pair>) x)
  ((primitive-set-field <pair> "cdr" <object>)
   p x))

(define (length list :: <list>) <int>
  (invoke-static <list> 'length list))
