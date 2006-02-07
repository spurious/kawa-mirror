(module-static #t)

(define (pair? x) :: <boolean>
  (<pair>:instance? x))

(define (cons car cdr) :: <pair>
  (<pair>:new car cdr))

(define (null? x) :: <boolean>
  (eq? x '()))

(define (set-car! (p <pair>) x)
  (set! p:car x))

(define (set-cdr! (p <pair>) x)
  ((primitive-set-field <pair> 'cdr <object>)
   p x))

(define-procedure car
  setter: set-car!
  (begin
    ;; Using just a lambda would not give it a name,
    ;; so it would use use "lambda" for a WrongType exception.
    (define (car (x :: <pair>))
      (*:.car x))
    car))

(define-procedure cdr
  setter: set-cdr!
  (begin
    (define (cdr (x :: <pair>))
      (field x 'cdr))
    cdr))

(define (length list :: <list>) :: <int>
  (invoke-static <list> 'length list))

(define (reverse (list :: <list>)) :: <list>
  (let loop ((arg list) (result '()))
    (if (null? arg) result
	(let ((pair :: <pair> arg))
	  (loop (cdr pair) (cons (car pair) result))))))

(define (list-tail list (count :: <int>))
  (<list>:listTail list count))

(define (list-ref list (index :: <int>))
  (car (list-tail list index)))

(define (list? obj) :: <boolean>
  (>= (invoke-static <list> 'listLength obj #f) 0))

;; Not in R5RS, but is in Guile (with extra mystery argument).
(define (reverse! (list :: <list>)) :: <list>
  (invoke-static <list> 'reverseInPlace list))

(define (memq x list)
  (let lp ((lst list))
    (and (instance? lst <pair>)
	 (if (eq? x ((primitive-get-field <pair> 'car <Object>) lst)) lst
	     (lp ((primitive-get-field <pair> 'cdr <Object>) lst))))))

(define (memv x list)
  (let lp ((lst list))
    (and (instance? lst <pair>)
	 (if (eqv? x ((primitive-get-field <pair> 'car <Object>) lst)) lst
	     (lp ((primitive-get-field <pair> 'cdr <Object>) lst))))))

;;;  The optional test argument is an srfi-1 extension.
(define (member x list #!optional (test :: <procedure> equal?))
  (let lp ((lst list))
    (and (instance? lst <pair>)
	 (if (test x ((primitive-get-field <pair> 'car <Object>) lst)) lst
	     (lp ((primitive-get-field <pair> 'cdr <Object>) lst))))))

(define (assq x list)
  (let lp ((list list))
    (if (eq? list '())
	 #f
	(let ((pair :: <pair>
		    ((primitive-get-field <pair> 'car <Object>) list)))
	  (if (eq? ((primitive-get-field <pair> 'car <Object>) pair) x)
	      pair
	      (lp ((primitive-get-field <pair> 'cdr <Object>) list)))))))

(define (assv x list)
  (let lp ((list list))
    (if (eq? list '())
	 #f
	(let ((pair :: <pair>
		    ((primitive-get-field <pair> 'car <Object>) list)))
	  (if (eqv? ((primitive-get-field <pair> 'car <Object>) pair) x)
	      pair
	      (lp ((primitive-get-field <pair> 'cdr <Object>) list)))))))

;;;  The optional test argument is an srfi-1 extension.
(define (assoc x list #!optional (test :: <procedure> equal?))
  (let lp ((list list))
    (if (eq? list '())
	 #f
	(let ((pair :: <pair>
		    ((primitive-get-field <pair> 'car <Object>) list)))
	  (if (test ((primitive-get-field <pair> 'car <Object>) pair) x)
	      pair
	      (lp ((primitive-get-field <pair> 'cdr <Object>) list)))))))
