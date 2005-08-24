(module-static #t)

(define (number? x) :: <boolean> (instance? x <number>))
(define (quantity? x) :: <boolean>  (instance? x <quantity>))
(define (complex? x) :: <boolean>  (instance? x <complex>))
(define (real? x) :: <boolean> (instance? x <real>))
(define (rational? x)  :: <boolean> (instance? x <rational>))
;;;(define (integer? x) ...)

(define (exact? x) :: <boolean> 
  (and (instance? x <number>) (invoke (as <number> x) 'isExact)))

(define (inexact? x) :: <boolean>
  (and (instance? x <number>) (not (invoke (as <number> x) 'isExact))))

(define (zero? (x :: <number>)) :: <boolean> 
  (invoke x 'isZero))

(define (positive? (x :: <real>)) :: <boolean>
  (> (invoke x 'sign) 0))

(define (negative? (x :: <real>)) :: <boolean> 
  (invoke x 'isNegative))

(define (odd? (x :: <integer>)) :: <boolean> 
  ((primitive-virtual-method <integer> "isOdd" <boolean> ())
   x))

(define (even? (x :: <integer>)) :: <boolean> 
  (not (odd? x)))

(define (abs (x :: <number>)) :: <number>
  (invoke x 'abs))

(define (quotient (x :: <real>) (y :: <real>)) :: <real>
  (if (and (instance? x <integer>) (instance? y <integer>))
      (invoke-static <integer> 'quotient x y)
      (invoke (/ x y) 'toInt (static-field <number> 'TRUNCATE))))

(define (remainder (x :: <real>) (y :: <real>)) :: <real> 
  (if (and (instance? x <integer>) (instance? y <integer>))
      (invoke-static <integer> 'remainder x y)
      (if (zero? y)
	  (if (exact? y) x (exact->inexact x))
	  (- x (* (invoke (/ x y) 'toInt (static-field <number> 'TRUNCATE))
		  y)))))

(define (modulo (x :: <real>) (y :: <real>)) :: <real>
  (if (and (instance? x <integer>) (instance? y <integer>))
      (invoke-static <integer> 'modulo x y)
      (if (zero? y)
	  (if (exact? y) x (exact->inexact x))
	  (- x (* (invoke (/ x y) 'toInt (static-field <number> 'FLOOR))
		  y)))))

(define (numerator (x :: <rational>)) :: <integer>
  (invoke x 'numerator))

(define (denominator (x :: <rational>)) :: <integer>
  (invoke x 'denominator))

(define (floor (x :: <real>)) :: <real>
  (invoke x 'toInt (static-field <number> 'FLOOR)))

(define (ceiling (x :: <real>)) :: <real>
  (invoke x 'toInt (static-field <number> 'CEILING)))

(define (truncate (x :: <real>)) :: <real>
  (invoke x 'toInt (static-field <number> 'TRUNCATE)))

(define (round (x :: <real>)) :: <real>
  (invoke x 'toInt (static-field <number> 'ROUND)))

(define (rationalize (x :: <real>) (y :: <real>)) :: <real>
  (gnu.math.RatNum:rationalize
   (as <real> (invoke x 'sub y))
   (as <real> (invoke x 'add y))))

(define (exp (x :: <complex>)) :: <complex>
  (invoke x 'exp))

(define (log (x :: <complex>)) :: <complex>
  (invoke x 'log))

;;; These are only implemented for <real> arguments.
(define (sin (x :: <double>)) :: <double>
  (invoke-static <java.lang.Math> 'sin x))

(define (cos (x :: <double>)) :: <double>
  (invoke-static <java.lang.Math> 'cos x))

(define (tan (x :: <double>)) :: <double>
  (invoke-static <java.lang.Math> 'tan x))

(define (asin (x :: <double>)) :: <double>
  (invoke-static <java.lang.Math> 'asin x))

(define (acos (x :: <double>)) :: <double>
  (invoke-static <java.lang.Math> 'acos x))

(define (sqrt (num :: <quantity>)) :: <quantity>
  (gnu.math.Quantity:make
   (invoke (invoke num 'number) 'sqrt)
   (invoke (invoke num 'unit) 'sqrt)))

(define (make-rectangular (x :: <real>) (y :: <real>)) :: <complex>
  (invoke-static <complex> 'make x y))

(define (make-polar (x :: <double>) (y :: <double>)) :: <gnu.math.DComplex>
  (invoke-static <complex> 'polar x y))

(define (real-part (x :: <complex>)) :: <real>
  (invoke x 're))

(define (imag-part (x :: <complex>)) :: <real>
  (invoke x 'im))

(define (magnitude (x :: <number>)) :: <number>
  (invoke x 'abs))

(define (angle (x :: <complex>)) :: <real>
  (invoke x 'angle))

(define (exact->inexact (num :: <number>)) :: <number>
  (if (invoke num 'isExact)
      (make <gnu.math.DFloNum> (invoke (as <real> num) 'doubleValue))
      num))

(define (inexact->exact (num :: <number>)) :: <number>
  (if (instance? num <real>)
      (invoke (as <real> num) 'toExact)
      num))

(define (arithmetic-shift (value :: <integer>) (amount :: <int>)) :: <integer>
  (invoke-static <integer> 'shift value amount))

(define (lognot (i :: <integer>)) :: <integer>
  (invoke-static <gnu.math.BitOps> 'not i))

(define (logop (op :: <int>) (i :: <integer>) (j :: <integer>)) :: <integer>
  (invoke-static <gnu.math.BitOps> 'bitOp op i j))

(define (logbit? (i :: <integer>) (bitno :: <int>)) :: <boolean>
  (invoke-static <gnu.math.BitOps> 'bitValue i bitno))

(define (bit-extract (i :: <integer>) (start :: <int>) (end :: <int>))
  :: <integer>
  (invoke-static <gnu.math.BitOps> 'extract i start end))

(define (logtest (i :: <integer>) (j :: <integer>))
  (invoke-static <gnu.math.BitOps> 'test i j))

(define (logcount (i :: <integer>)) :: <int>
  (invoke-static <gnu.math.BitOps> 'bitCount i))

(define (integer-length (i :: <integer>)) :: <int>
  (invoke i 'intLength))

(define (number->string (arg :: <number>) #!optional (radix :: <int> 10))
  (make <string>
    (invoke arg 'toString radix)))
