(define (number? x) (instance? x <number>))
(define (quantity? x) (instance? x <quantity>))
(define (complex? x) (instance? x <complex>))
(define (real? x) (instance? x <real>))
(define (rational? x) (instance? x <rational>))
;;;(define (integer? x) ...)

(define (zero? x)
  ((primitive-virtual-method <number> "isZero" <boolean> ())
   x))
(define (abs x)
  ((primitive-virtual-method <number> "abs" <number> ())
   x))
(define (quotient x y)
  ((primitive-static-method <integer> "quotient" <integer>
			    (<integer> <integer>))
   x y))
(define (remainder x y)
  ((primitive-static-method <integer> "remainder" <integer>
			    (<integer> <integer>))
   x y))

(define (numerator x)
  ((primitive-virtual-method <rational> "numerator" <integer> ()) x))
(define (denominator x)
  ((primitive-virtual-method <rational> "denominator" <integer> ()) x))

(define (exp x)
  ((primitive-virtual-method <complex> "exp" <complex> ())  x))
(define (log x)
  ((primitive-virtual-method <complex> "log" <complex> ())  x))

(define (make-rectangular x y)
  ((primitive-static-method <complex> "make" <complex> (<real> <real>))  x y))
(define (make-polar x y)
  ((primitive-static-method <complex> "polar" <gnu.math.DComplex>
			    (<double> <double>))
   x y))
(define (real-part x)
  ((primitive-virtual-method <complex> "re" <real> ())  x))

