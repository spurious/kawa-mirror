(module-static #t)

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

(define-procedure car
  setter: set-car!
  (lambda ((x :: <pair>))
    ((primitive-get-field <pair> "car" <Object>)
     x)))

(define-procedure cdr
  setter: set-cdr!
  (lambda ((x :: <pair>))
    ((primitive-get-field <pair> "cdr" <Object>)
     x)))

(define (length list :: <list>) <int>
  (invoke-static <list> 'length list))
