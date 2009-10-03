#| The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   Contributed by Per Bothner.
   Based on (i.e. a fairly direct transcription of) the "Java 6 -server"
   version contributed by Stefan Krause and slightly modified by Chad Whipkey.
|#
(define size :: int (string->number (cadr (command-line))))
(define fac :: double (/ 2.0 size))
(define shift :: int (let ((sz8 :: int (mod size 8)))
		       (if (= sz8 0) 0 (- 8 sz8))))
(define out :: java.io.PrintStream java.lang.System:out)
(define-constant buffer-size :: int 8192)
(define bbuffer :: byte[] (byte[] length: buffer-size))
(define buf-len :: int 0)

(define (compute-row (y :: int)) :: void
  (let* ((bits :: int 0)
         (Ci :: double (- (* y fac) 1.0))
	 (buf bbuffer)) ;; Minor optimization.
    (do ((x :: int 0 (+ x 1)))
	((>= x size))
      (let ((Zr :: double 0.0)
            (Zi :: double 0.0)
            (Cr :: double (- (* x fac) 1.5))
	    (ZrN :: double 0)
	    (ZiN :: double 0)
	    (i :: int 50))
	(let loop ()
	  (set! Zi (+ (* 2.0 Zr Zi) Ci))
	  (set! Zr (+ (- ZrN ZiN) Cr))
	  (set! ZiN (* Zi Zi))
	  (set! ZrN (* Zr Zr))
	  (cond ((<= (+ ZiN ZrN) 4.0)
		 (set! i (- i 1))
		 (if (> i 0)
		     (loop)))))

	(set! bits (bitwise-arithmetic-shift bits 1))
	(if (= i 0)
	    (set! bits (+ bits 1)))

	(cond ((= (remainder x 8) 7)
	       (set! (buf buf-len) bits)
	       (set! buf-len (+ buf-len 1))
	       (cond ((= buf-len buffer-size)
		      (out:write buf 0 buffer-size)
		      (set! buf-len 0)))
	       (set! bits 0)))))
    (cond ((> shift 0)
	   (set! bits (bitwise-arithmetic-shift bits shift))
	   (set! (buf buf-len) bits)
	   (set! buf-len (+ buf-len 1))
	   (cond ((= buf-len buffer-size)
		  (out:write buf 0 buffer-size)
		  (set! buf-len 0)))))))

(define (compute) :: void
  (let ((sz :: java.lang.Integer size))
    (out:format "P4\n%d %d\n" sz sz))
  (do ((y :: int 0 (+ y 1)))
      ((>= y size))
    (compute-row y))
  (out:write bbuffer 0 buf-len)
  (out:close))

(compute)
