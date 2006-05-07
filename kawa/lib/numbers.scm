(module-static #t)

(define (number? x) :: <boolean> (instance? x <number>))
(define (quantity? x) :: <boolean>  (instance? x <quantity>))
(define (complex? x) :: <boolean>  (instance? x <complex>))
(define (real? x) :: <boolean> (instance? x <real>))
(define (rational? x)  :: <boolean> (instance? x <rational>))
(define (integer? x) :: <boolean>
  (or (instance? x <gnu.math.IntNum>)
      (and (instance? x <gnu.math.DFloNum>)
	   (= (java.lang.Math:IEEEremainder
	       (gnu.math.DFloNum:doubleValue x)
	       1.0)
	      0.0))))

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

(define (max #!rest (args :: <Object[]>))
  (let ((n :: <int> args:length)
	(result :: <real> (args 0)))
    (do ((i :: <int> 1 (+ i 1)))
	 ((>= i n) result)
      (set! result
	    (*:max result (args i))))))

(define (min #!rest (args :: <Object[]>))
  (let ((n :: <int> args:length)
	(result :: <real> (args 0)))
    (do ((i :: <int> 0 (+ i 1)))
	 ((>= i n) result)
      (set! result
	    (*:min result (args i))))))

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

(define (gcd #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	0
	(let ((result :: <integer> (args 0)))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (<integer>:gcd result (<integer>:@ (args i)))))))))

(define (lcm #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	1
	(let ((result :: <integer> (<integer>:abs (<integer>:@ (args 0)))))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (<integer>:lcm result (<integer>:@ (args i)))))))))

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

(define (logand #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	-1
	(let ((result :: <integer> (args 0)))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (let ((arg-i :: <integer> (args i)))
	      (set! result (gnu.math.BitOps:and result arg-i))))))))

(define (logior #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	0
	(let ((result :: <integer> (args 0)))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (gnu.math.BitOps:ior result (args i))))))))

(define (logxor #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	0
	(let ((result :: <integer> (args 0)))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (gnu.math.BitOps:xor result (args i))))))))

(define (logtest (i :: <integer>) (j :: <integer>))
  (invoke-static <gnu.math.BitOps> 'test i j))

(define (logcount (i :: <integer>)) :: <int>
  (invoke-static <gnu.math.BitOps> 'bitCount i))

(define (integer-length (i :: <integer>)) :: <int>
  (invoke i 'intLength))

(define (number->string (arg :: <java.lang.Number>)
			#!optional (radix :: <int> 10))
  (make <string>
    (gnu.kawa.functions.Arithmetic:toString arg radix)))

(define (quantity->number (q :: <quantity>)) :: <complex>
  (let ((u (q:unit))
	(factor (q:doubleValue)))
    (if (= factor 1.0)
	(q:number)
	(<complex>:make (q:reValue) (q:imValue)))))

(define (quantity->unit (q :: <quantity>)) :: <gnu.math.Unit>
  (q:unit))

(define (make-quantity val unit) :: <quantity>
  (let ((u :: <gnu.math.Unit>
	   (if (instance? unit <gnu.math.Unit>) unit
	       (<gnu.math.Unit>:lookup unit))))
    (if (eq? u #!null)
	(primitive-throw (<java.lang.IllegalArgumentException>
			 (format "unknown unit: ~s" unit))))
    (<quantity>:make val u)))

(define (duration duration) :: <gnu.math.Duration>
  (gnu.math.Duration:parseDuration duration))
