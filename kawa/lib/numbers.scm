(define (number? x) (instance? x <number>))
(define (quantity? x) (instance? x <quantity>))
(define (complex? x) (instance? x <complex>))
(define (real? x) (instance? x <real>))
(define (rational? x) (instance? x <rational>))
;;;(define (integer? x) ...)

(define (zero? (x <number>))
  ((primitive-virtual-method <number> "isZero" <boolean> ())
   x))
(define (negative? (x <real>))
  ((primitive-virtual-method <real> "isNegative" <boolean> ())
   x))
(define (odd? (x <integer>))
  ((primitive-virtual-method <integer> "isOdd" <boolean> ())
   x))
(define (even? (x <integer>))
  (not (odd? x)))

(define (abs (x <number>))
  ((primitive-virtual-method <number> "abs" <number> ())
   x))
(define (quotient (x <integer>) (y <integer>))
  ((primitive-static-method <integer> "quotient" <integer>
			    (<integer> <integer>))
   x y))
(define (remainder x y)
  ((primitive-static-method <integer> "remainder" <integer>
			    (<integer> <integer>))
   x y))

(define (numerator (x <integer>) (y <integer>))
  ((primitive-virtual-method <rational> "numerator" <integer> ()) x))

(define (denominator (x <integer>))
  ((primitive-virtual-method <rational> "denominator" <integer> ()) x))

(define (exp (x <complex>))
  ((primitive-virtual-method <complex> "exp" <complex> ())  x))
(define (log (x <complex>))
  ((primitive-virtual-method <complex> "log" <complex> ())  x))

;;; These are only implemented for <real> arguments.
(define (sin (x <real>))
  (invoke-static <java.lang.Math> "sin" x))
;                 ((primitive-virtual-method <java.lang.Number> "doubleValue" <double> ())
;                  x)))
(define (cos (x <real>))
  (invoke-static <java.lang.Math> "cos"
                 ((primitive-virtual-method <java.lang.Number> "doubleValue" <double> ())
                  x)))
(define (tan (x <real>))
  (invoke-static <java.lang.Math> "tan"
                 ((primitive-virtual-method <java.lang.Number> "doubleValue" <double> ())
                  x)))

(define (asin (x :: <double>))
  (invoke-static <java.lang.Math> 'asin x))

(define (acos (x :: <double>))
  (invoke-static <java.lang.Math> 'acos x))

(define (make-rectangular (x <real>) (y <real>))
  ((primitive-static-method <complex> "make" <complex> (<real> <real>))  x y))
(define (make-polar (x <double>) (y <double>))
  ((primitive-static-method <complex> "polar" <gnu.math.DComplex>
			    (<double> <double>))
   x y))
(define (real-part (x <complex>))
  ((primitive-virtual-method <complex> "re" <real> ())  x))
(define (imag-part (x <complex>))
  ((primitive-static-method <quantity> "make" <quantity>
			    (<complex> <gnu.math.Unit>))
   ((primitive-virtual-method <quantity> "im" <real> ())  x)
   ((primitive-virtual-method <quantity> "unit" <gnu.math.Unit> ()) x)))
(define (magnitude (x <number>))
  ((primitive-virtual-method <number> "abs" <number> ())
   x))
(define (angle (x <complex>))
  ((primitive-virtual-method <complex> "angle" <real>
 ())
   ((primitive-virtual-method <quantity> "number" <complex> ()) x)))

(define (lognot (i :: <integer>))
  (invoke-static <gnu.math.BitOps> 'not i))

(define (logop (op :: <int>) (i :: <integer>) (j :: <integer>))
  (invoke-static <gnu.math.BitOps> 'bitOp op i j))

(define (logbit? (i :: <integer>) (bitno :: <int>))
  (invoke-static <gnu.math.BitOps> 'bitValue i bitno))

(define (bit-extract (i :: <integer>) (start :: <int>) (end :: <int>))
  (invoke-static <gnu.math.BitOps> 'extract i start end))

(define (logtest (i :: <integer>) (j :: <integer>))
  (invoke-static <gnu.math.BitOps> 'test i j))

(define (logcount (i :: <integer>))
  (invoke-static <gnu.math.BitOps> 'bitCount i))

(define (integer-length (i :: <integer>))
  (invoke i 'intLength))
