(module-static #t)

(define (pair? x) :: <boolean>
  (instance? x <pair>))

(define (cons car cdr) :: <pair>
  (make <pair> car cdr))

(define (null? x) :: <boolean>
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

(define (length list :: <list>) :: <int>
  (invoke-static <list> 'length list))

(define (reverse (list :: <list>)) :: <list>
  (let loop ((arg list) (result '()))
    (if (null? arg) result
	(let ((pair :: <pair> arg))
	  (loop (cdr pair) (cons (car pair) result))))))

;; Not in R5RS, but is in Guile (with extra mystery argument).
(define (reverse! (list :: <list>)) :: <list>
  (invoke-static <list> 'reverseInPlace list))
