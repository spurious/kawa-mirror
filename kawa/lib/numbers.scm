(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.misc>)

(define (number? x) :: <boolean> (java.lang.Number? x))
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
	      0.0))
      (and (instance? x <java.lang.Number>)
	   (or (instance? x <java.lang.Long>)
	       (instance? x <java.lang.Integer>)
	       (instance? x <java.lang.Short>)
	       (instance? x <java.math.BigInteger>)))))

(define (exact? x) :: boolean 
  (and (java.lang.Number? x)
       (gnu.kawa.functions.Arithmetic:isExact (as java.lang.Number x))))

(define (inexact? x) :: boolean
  (and (java.lang.Number? x)
       (not (gnu.kawa.functions.Arithmetic:isExact (as java.lang.Number x)))))

(define (zero? (x :: java.lang.Number)) :: boolean
  (cond ((gnu.math.Numeric? x)
	 ((as gnu.math.Numeric x):isZero))
	((java.math.BigInteger? x)
	 (= 0 (as java.math.BigInteger x):signum))
	((java.math.BigDecimal? x)
	 (= 0 (as java.math.BigDecimal x):signum))
	(else
	 (= 0.0 (x:doubleValue)))))

(define (positive? (x :: <real>)) :: <boolean>
  (> (invoke x 'sign) 0))

(define (negative? (x :: real)) :: <boolean> 
  (invoke x 'isNegative))

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

(define (abs (x :: java.lang.Number)) :: java.lang.Number
  (cond ((gnu.math.Numeric? x)
	 ((as gnu.math.Numeric x):abs))
	((>= x 0)
	 x)
	(else
	 (- x))))

(define (div-and-mod (x :: real) (y :: real))
  (let* ((q (div x y))
	 (r (- x (* q y))))
    (values q r)))

(define (div0-and-mod0 (x :: real) (y :: real))
  (let* ((q (div0 x y))
	 (r (- x (* q y))))
    (values q r)))

(define (gcd #!rest (args ::integer[])) :: integer
  (let ((n args:length))
    (if (= n 0)
	0
	(let ((result ::integer (args 0)))
	  (do ((i ::int 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (gnu.math.IntNum:gcd result (args i))))))))

(define (lcm #!rest (args ::integer[])) :: <integer>
  (let ((n args:length))
    (if (= n 0)
	1
	(let ((result ::integer (gnu.math.IntNum:abs (args 0))))
	  (do ((i ::int 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (gnu.math.IntNum:lcm result (args i))))))))

(define (numerator (x :: <rational>)) :: <integer>
  (x:numerator))

(define (denominator (x :: <rational>)) :: <integer>
  (x:denominator))

(define (floor (x :: real)) :: real
  (x:toInt gnu.math.Numeric:FLOOR))

(define (ceiling (x :: real)) :: real
  (x:toInt gnu.math.Numeric:CEILING))

(define (truncate (x :: real)) :: real
  (x:toInt gnu.math.Numeric:TRUNCATE))

(define (round (x :: real)) :: real
  (x:toInt gnu.math.Numeric:ROUND))

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

(define-procedure atan
  (lambda ((y :: <double>) (x :: <double>)) :: <double>
	  (java.lang.Math:atan2 y x))
  (lambda ((x :: <double>)) :: <double>
	  (java.lang.Math:atan x)))

(define-procedure sqrt
  (lambda ((num :: <quantity>)) :: <quantity>
	  (gnu.math.Quantity:make
	   (invoke (invoke num 'number) 'sqrt)
	   (invoke (invoke num 'unit) 'sqrt)))
  (lambda ((x :: <double>)) :: <double>
	  (java.lang.Math:sqrt x)))

(define (make-rectangular (x :: <real>) (y :: <real>)) :: <complex>
  (invoke-static <complex> 'make x y))

(define (make-polar (x :: <double>) (y :: <double>)) :: <gnu.math.DComplex>
  (invoke-static <complex> 'polar x y))

(define (real-part (x :: <complex>)) :: <real>
  (invoke x 're))

(define (imag-part (x :: <complex>)) :: <real>
  (invoke x 'im))

(define (magnitude (x :: java.lang.Number)) :: java.lang.Number
  (abs x))

(define (angle (x :: <complex>)) :: <real>
  (invoke x 'angle))

(define (inexact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toInexact num))

(define (exact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toExact num))

(define (exact->inexact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toInexact num))

(define (inexact->exact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toExact num))

(define (logop (op :: <int>) (i :: <integer>) (j :: <integer>)) :: <integer>
  (invoke-static <gnu.math.BitOps> 'bitOp op i j))

(define (bitwise-bit-set? (i :: <integer>) (bitno :: <int>)) :: <boolean>
  (gnu.math.BitOps:bitValue i bitno))

(define (bitwise-copy-bit (i :: integer) (bitno :: int) (new-value :: int))
  :: integer
  (gnu.math.BitOps:setBitValue i bitno new-value))

(define (bitwise-copy-bit-field (to :: integer) (start :: int) (end :: int) (from :: integer)) ::  integer
  (let* ((mask1 (gnu.math.IntNum:shift -1 start))
	 (mask2 (gnu.math.BitOps:not (gnu.math.IntNum:shift -1 end)))
	 (mask (gnu.math.BitOps:and mask1 mask2)))
    (bitwise-if mask
		(gnu.math.IntNum:shift from start)
		to)))

(define (bitwise-bit-field (i :: <integer>) (start :: <int>) (end :: <int>))
  :: <integer>
  (invoke-static <gnu.math.BitOps> 'extract i start end))

(define (bitwise-if (e1 :: integer) (e2 :: integer) (e3  integer)) :: integer
  (gnu.math.BitOps:ior (gnu.math.BitOps:and e1 e2)
		       (gnu.math.BitOps:and (gnu.math.BitOps:not e1) e3)))

(define (logtest (i :: <integer>) (j :: <integer>))
  (invoke-static <gnu.math.BitOps> 'test i j))

(define (logcount (i :: <integer>)) :: <int>
  (gnu.math.BitOps:bitCount
   (if (>= i 0) i (gnu.math.BitOps:not i))))

(define (bitwise-bit-count (i :: <integer>)) :: <int>
  (if (>= i 0)
      (gnu.math.BitOps:bitCount i)
      (- -1 (gnu.math.BitOps:bitCount (gnu.math.BitOps:not i)))))  

(define (bitwise-length (i :: <integer>)) :: <int>
  (invoke i 'intLength))

(define (bitwise-first-bit-set (i :: <integer>)) :: <int>
  (gnu.math.BitOps:lowestBitSet i))

(define (bitwise-rotate-bit-field (n :: integer) (start :: int) (end :: int) (count :: int)) :: integer
  (let ((width (- end start)))
    (if (> width 0)
	(let* (;; Optimization of modulo.
	       (r (remainder count width))
	       (count (if (< r 0) (+ r width) r))
	       (field0 (bitwise-bit-field n start end))
	       (field1 (gnu.math.IntNum:shift field0 count))
	       (field2 (gnu.math.IntNum:shift field0 (- count width)))
	       (field (gnu.math.BitOps:ior field1 field2)))
	  (bitwise-copy-bit-field n start end field))
	n)))

(define (bitwise-reverse-bit-field (n :: integer) (start :: int) (end :: int)) :: integer
  (gnu.math.BitOps:reverseBits n start end))

(define (number->string (arg :: <java.lang.Number>)
			#!optional (radix :: <int> 10)) ::string
  (make <gnu.lists.FString>
    (gnu.kawa.functions.Arithmetic:toString arg radix)))

(define (string->number (str :: <string>) #!optional (radix :: <int> 10))
  ::object
  (let ((result (gnu.kawa.lispexpr.LispReader:parseNumber str radix)))
    (if (instance? result <gnu.math.Numeric>) result #f)))

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
			 ((format "unknown unit: ~s" unit):toString))))
    (<quantity>:make val u)))

(define (duration duration) :: <gnu.math.Duration>
  (gnu.math.Duration:parseDuration duration))
