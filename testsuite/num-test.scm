(TEST-INIT "numbers")

;; A problem posed by Ken Dickey (kend@data.UUCP) on comp.lang.lisp
;; to check numerical exactness of Lisp implementations.
(define (dickey-test x y)
  (+  (* 1335/4 (expt y 6))
      (* (expt x 2)
	 (- (* 11 (expt x 2) (expt y 2))
	    (expt y 6)
	    (* 121 (expt y 4))
	    2))
      (* 11/2 (expt y 8))
      (/ x (* 2 y))))
(test -54767/66192 dickey-test 77617 33096)

(test 0 + 17280012451545786657095548928 -17280012451545786657095548928)
(test 1250120440709706990357803482218496
  + 1250137720722158536144460577767424 -17280012451545786657095548928)
(test 100000000000000 quotient
   10000000000000000000000000000000000 100000000000000000000)
(test 1250120440709706990357803482218496
  - 1250137720722158536144460577767424 17280012451545786657095548928)
(test -1250120440709706990357803482218496
  - 17280012451545786657095548928 1250137720722158536144460577767424)

(SECTION "expt")
(test 9223372036854775808 expt 2 63)

(SECTION "convert")
(test 10000000000 inexact->exact (exact->inexact 10000000000))
(test 0 inexact->exact 0.0)
(Test 123451/10 rationalize (inexact->exact 12345.1) (inexact->exact 0.00001))

(SECTION "magnitude")
(test 4.0 magnitude 4.)
(test 4e3 magnitude -4000.)
(test 5.0 magnitude 3-4i)
(test 3/2 magnitude (/ 6 -4))

(SECTION "shift")
(test 12676506002282294014967032053760 arithmetic-shift 10 100)

(SECTION "logcount")
(test 3 logcount 13)
(test 2 logcount -13)
(test 4 logcount 30)
(test 4 logcount -30)

(SECTION "logop")

;; A Boolean 1-bit version of logop.
(define (logop-bits op x y)
  (odd? (quotient op (* (if x 1 4) (if y 1 2)))))

(define (logop-compare result op x y)
  (do ((i 0 (+ i 1)))
      ((or (= i 100)
	   (not (eq? (logop-bits op (logbit? x i) (logbit? y i))
		     (logbit? result i))))
       i)
    #t))

(define (logop-test1 op x y)
  (logop-compare (logop op x y) op x y))

(define test-vals '(0 1 -1 2 -2 3 -3 #x7fffffff
		      #x-f0f0cccc12345 #x1234567890abcdef0012345))

(define (logop-test op)
  (do ((xl test-vals (cdr xl)))
      ((null? xl) #t)
    (do ((yl test-vals (cdr yl)))
      ((null? yl) #t)
      (test 100 logop-test1 op (car xl) (car yl)))))

(do ((i 0 (+ i 1)))
    ((= i 16) #t)
  (logop-test i))

(SECTION "integer-length")

(test 0 integer-length 0)
(test 1 integer-length 1)
(test 2 integer-length 3)
(test 3 integer-length 4)
(test 3 integer-length 7)
(test 0 integer-length -1)
(test 2 integer-length -4)
(test 3 integer-length -7)
(test 3 integer-length -8)
(test 31 integer-length #x7fffffff)
(test 32 integer-length #xffffffff)
(test 33 integer-length #x100000000)
(test 1000000000000000000000000000000 * 1000000000000000 1000000000000000)
