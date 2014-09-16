(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.misc>)

(define-private (java.lang.real? x) ::boolean
  (and (java.lang.Number? x)
       (or (java.lang.Long? x)
           (java.lang.Integer? x)
           (java.lang.Short? x)
           (java.lang.Byte? x)
           (java.lang.Double? x)
           (java.lang.Float? x)
           (java.math.BigInteger? x)
           (java.math.BigDecimal? x))))
(define (number? x) ::boolean (java.lang.Number? x))
(define (quantity? x) ::boolean
  (or (instance? x <quantity>)
      (java.lang.real? x)))
(define (complex? x) ::boolean
  (or (instance? x <complex>)
      (java.lang.real? x)))
(define (real? x) ::boolean
  (or (instance? x <real>)
      (java.lang.real? x)))
(define (rational? x)  ::boolean
  (or (instance? x <rational>)
      (and (java.lang.Number? x)
	   (or (java.lang.Long? x)
	       (java.lang.Integer? x)
	       (java.lang.Short? x)
	       (java.lang.Byte? x)
	       (java.math.BigInteger? x)
	       (java.math.BigDecimal? x)))))

(define (integer? x) :: <boolean>
  (or (instance? x <gnu.math.IntNum>)
      (and (instance? x <java.lang.Number>)
           (cond ((or (java.lang.Long? x)
                      (java.lang.Integer? x)
                      (java.lang.Short? x)
                      (java.lang.Byte? x)
                      (java.math.BigInteger? x))
                  #t)
                 ((or (gnu.math.DFloNum? x)
                      (java.lang.Float? x)
                      (java.lang.Double? x))
                  (= (java.lang.Math:IEEEremainder
                      (java.lang.Number:doubleValue x)
                      1.0)
                     0.0))
                 ((java.math.BigDecimal? x)
                  (try-catch
                   (begin
                     ((->java.math.BigDecimal x):toBigIntegerExact)
                     #t)
                   (ex java.lang.ArithmeticException #f)))))))

(define (exact-integer? x) :: <boolean>
  (or (instance? x <gnu.math.IntNum>)
      (and (instance? x <java.lang.Number>)
	   (or (instance? x <java.lang.Long>)
	       (instance? x <java.lang.Integer>)
	       (instance? x <java.lang.Short>)
	       (instance? x <java.lang.Byte>)
	       (instance? x <java.math.BigInteger>)))))

(define (real-valued? x) ::boolean
  (and (complex? x) (zero? (imag-part x)) (real? (real-part x))))
(define (rational-valued? x) ::boolean
  (and (complex? x) (zero? (imag-part x)) (rational? (real-part x))))
(define (integer-valued? x) ::boolean
  (and (complex? x) (zero? (imag-part x)) (integer? (real-part x))))

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

(define (finite? (z ::java.lang.Number)) ::boolean
  (if (gnu.math.Complex? z)
      (> ((->gnu.math.Complex z):classifyFinite) 0)
      (and (java.lang.real? z)
           (let ((d (z:doubleValue)))
             (and (not (java.lang.Double:isInfinite d))
                  (not (java.lang.Double:isNaN d)))))))

(define (infinite? (z ::java.lang.Number)) ::boolean
  (if (gnu.math.Complex? z)
      (let ((zc ::gnu.math.Complex z))
        (or (= ((zc:re):classifyFinite) 0)
            (= ((zc:im):classifyFinite) 0)))
      (and (java.lang.real? z)
           (let ((d (z:doubleValue)))
             (java.lang.Double:isInfinite d)))))

(define (nan? (z ::java.lang.Number)) ::boolean
  (if (gnu.math.Complex? z)
      (< ((->gnu.math.Complex z):classifyFinite) 0)
      (and (java.lang.real? z)
           (let ((d (z:doubleValue)))
             (java.lang.Double:isNaN d)))))

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

(define (floor/ (x :: real) (y :: real))
  (let* ((q (floor-quotient x y))
	 (r (- x (* q y))))
    (values q r)))

(define (truncate/ (x :: real) (y :: real))
  (let* ((q (truncate-quotient x y))
	 (r (- x (* q y))))
    (values q r)))

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

(define-procedure log
  (lambda ((x ::complex) (base ::complex))  ::complex
          (/ (invoke x 'log) (invoke base 'log)))
  (lambda ((x ::complex))  ::complex
          (invoke x 'log)))

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
#|
  ;; Only when the "required type" is real:
  (lambda ((x :: <double>)) :: <double>
	  (java.lang.Math:sqrt x))
|#
  (lambda ((num :: <quantity>)) :: <quantity>
	  (gnu.math.Quantity:make
	   (invoke (invoke num 'number) 'sqrt)
	   (invoke (invoke num 'unit) 'sqrt))))

(define (square x::quantity) ::quantity
  (* x x))

(define (make-rectangular (x :: <real>) (y :: <real>)) :: <complex>
  (invoke-static <complex> 'make x y))

(define (make-polar (x :: <double>) (y :: <double>)) :: <gnu.math.DComplex>
  (invoke-static <complex> 'polar x y))

(define (real-part (x ::java.lang.Number)) ::java.lang.Number
  (if (gnu.math.Complex? x)
      ((->gnu.math.Complex x):re)
      x))

(define (imag-part (x ::java.lang.Number)) ::java.lang.Number
  (if (gnu.math.Complex? x)
      ((->gnu.math.Complex x):im)
      (gnu.math.IntNum:zero)))

(define (magnitude (x :: java.lang.Number)) :: java.lang.Number
  (abs x))

(define (angle (x ::java.lang.Number)):: <real>
  (if (gnu.math.Complex? x)
      ((->gnu.math.Complex x):angle)
      (if (< (x:doubleValue) 0) java.lang.Math:PI 0)))

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

(define (string->number (str :: <string>) #!optional (radix ::int 10))
  ::object
  (let ((result (gnu.kawa.lispexpr.LispReader:parseNumber str (- radix))))
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

#| The algorithm of exact-integer-sqrt is from chibi-scheme,
which has:
Copyright (c) 2009-2012 Alex Shinn
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(define (exact-integer-sqrt (i ::integer))
  (if (< i 0)
      (primitive-throw (java.lang.IllegalArgumentException
                        (format #f "negative argument: ~A" i)))
      (let ((res (sqrt i)))
        (let lp ((res ::integer (inexact->exact (truncate res))))
          (let ((rem ::integer (- i (* res res))))
            (if (negative? rem)
                (lp (quotient (+ res (quotient i res)) 2))
                (values res rem)))))))
