(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)

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
	      0.0))
      (and (instance? x <java.lang.Number>)
	   (or (instance? x <java.lang.Long>)
	       (instance? x <java.lang.Integer>)
	       (instance? x <java.lang.Short>)
	       (instance? x <java.math.BigInteger>)))))

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

(define-procedure quotient
  ;; (x :: <real>) (y :: <real>)) :: <real>
  (lambda ((x :: <integer>) (y :: <integer>)) :: <integer>
	  (invoke-static <integer> 'quotient x y))
  (lambda ((x :: <real>) (y :: <real>)) :: <real>
	  (invoke (/ x y) 'toInt (static-field <number> 'TRUNCATE))))

(define-procedure remainder
  ;; (x :: <real>) (y :: <real>)) :: <real> 
  (lambda ((x :: <integer>) (y :: <integer>)) :: <integer>
	  (invoke-static <integer> 'remainder x y))
  (lambda ((x :: <real>) (y :: <real>)) :: <real>
	  (if (zero? y)
	      (if (exact? y) x (exact->inexact x))
	      (- x (* (invoke (/ x y) 'toInt (static-field <number> 'TRUNCATE))
		      y)))))

(define-procedure modulo
  ;; (x :: <real>) (y :: <real>)) :: <real>
  (lambda ((x :: <integer>) (y :: <integer>)) :: <integer>
	  (invoke-static <integer> 'modulo x y))
  (lambda ((x :: <real>) (y :: <real>)) :: <real>
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
	    (set! result (gnu.math.IntNum:gcd result (<integer>:@ (args i)))))))))

(define (lcm #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	1
	(let ((result :: <integer> (gnu.math.IntNum:abs (<integer>:@ (args 0)))))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (gnu.math.IntNum:lcm result (<integer>:@ (args i)))))))))

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

(define (bitwise-arithmetic-shift (value :: <integer>) (amount :: <int>)) :: <integer>
  (gnu.math.IntNum:shift value amount))

(define (bitwise-arithmetic-shift-left (value :: <integer>) (amount :: <int>)) :: <integer>
  (if (< amount 0) (throw 'misc-error "shift amount must be non-negative"))
  (gnu.math.IntNum:shift value amount))

(define (bitwise-arithmetic-shift-right (value :: <integer>) (amount :: <int>)) :: <integer>
  (if (< amount 0) (throw 'misc-error "shift amount must be non-negative"))
  (gnu.math.IntNum:shift value (- amount)))

(define (bitwise-not (i :: <integer>)) :: <integer>
  (gnu.math.BitOps:not i))

(define (logop (op :: <int>) (i :: <integer>) (j :: <integer>)) :: <integer>
  (invoke-static <gnu.math.BitOps> 'bitOp op i j))

(define (bitwise-bit-set? (i :: <integer>) (bitno :: <int>)) :: <boolean>
  (gnu.math.BitOps:bitValue i bitno))

(define (bitwise-copy-bit (i :: integer) (bitno :: int) (new-value :: int))
  :: integer
  (gnu.math.BitOps:setBitValue i bitno new-value))

(define (bitwise-copy-bit-field (to :: integer) (start :: int) (end :: int) (from :: integer)) ::  integer
  (let* ((mask1 (bitwise-arithmetic-shift-left -1 start))
	 (mask2 (bitwise-not (bitwise-arithmetic-shift-left -1 end)))
	 (mask (bitwise-and mask1 mask2)))
    (bitwise-if mask
		(bitwise-arithmetic-shift-left from start)
		to)))

(define (bitwise-bit-field (i :: <integer>) (start :: <int>) (end :: <int>))
  :: <integer>
  (invoke-static <gnu.math.BitOps> 'extract i start end))

(define (bitwise-and #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	-1
	(let ((result :: <integer> (args 0)))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (let ((arg-i :: <integer> (args i)))
	      (set! result (gnu.math.BitOps:and result arg-i))))))))

(define (bitwise-ior #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	0
	(let ((result :: <integer> (args 0)))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (gnu.math.BitOps:ior result (args i))))))))

(define (bitwise-xor #!rest (args :: <Object[]>)) :: <integer>
  (let ((n :: <int> args:length))
    (if (zero? n)
	0
	(let ((result :: <integer> (args 0)))
	  (do ((i :: <int> 1 (+ i 1)))
	      ((>= i n) result)
	    (set! result (gnu.math.BitOps:xor result (args i))))))))

(define (bitwise-if (e1 :: integer) (e2 :: integer) (e3  integer)) :: integer
  (bitwise-ior (bitwise-and e1 e2)
	       (bitwise-and (bitwise-not e1) e3)))

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
    (if (positive? width)
	(let* ((count (modulo count width))
	       (field0 (bitwise-bit-field n start end))
	       (field1 (bitwise-arithmetic-shift-left field0 count))
	       (field2 (bitwise-arithmetic-shift-right
			field0
			(- width count)))
	       (field (bitwise-ior field1 field2)))
	  (bitwise-copy-bit-field n start end field))
	n)))

(define (bitwise-reverse-bit-field (n :: integer) (start :: int) (end :: int)) :: integer
  (gnu.math.BitOps:reverseBits n start end))

(define (number->string (arg :: <java.lang.Number>)
			#!optional (radix :: <int> 10))
  (make <gnu.lists.FString>
    (gnu.kawa.functions.Arithmetic:toString arg radix)))

(define (string->number (str :: <string>) #!optional (radix :: <int> 10))
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
