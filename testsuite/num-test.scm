(test-init "numbers" 1656)

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

(test -1/10000000000000 / -1 #e1e13)
(test 9223372036854775808 - (- (expt 2 63)))
(test #i1/0 + 1e4294967297)
(test #i1/0 * 1e429496729742942949672974967297 1)

(test 500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      quotient (expt 10 200) (* 20 (expt 10 100)))

(test 0 + 17280012451545786657095548928 -17280012451545786657095548928)
(test 1250120440709706990357803482218496
  + 1250137720722158536144460577767424 -17280012451545786657095548928)
(test 100000000000000 quotient
   10000000000000000000000000000000000 100000000000000000000)
(test 1250120440709706990357803482218496
  - 1250137720722158536144460577767424 17280012451545786657095548928)
(test -1250120440709706990357803482218496
  - 17280012451545786657095548928 1250137720722158536144460577767424)

(section "expt")
(test 9223372036854775808 expt 2 63)

(section "convert")
(test 10000000000 inexact->exact (exact->inexact 10000000000))
(test 1.4285714285714286e22 exact->inexact 14285714285714285714285)
(test 0 inexact->exact 0.0)
(test 123451/10 rationalize (inexact->exact 12345.1) (inexact->exact 0.00001))

(section "magnitude")
(test 4.0 magnitude 4.)
(test 4e3 magnitude -4000.)
(test 5.0 magnitude 3-4i)
(test 3/2 magnitude (/ 6 -4))

(section "shift")
(test 12676506002282294014967032053760 arithmetic-shift 10 100)

(section "logcount")
(test 3 logcount 13)
(test 2 logcount -13)
(test 4 logcount 30)
(test 4 logcount -30)

(section "gcd")
(test 3 gcd 4294967295 3)
(test 3 gcd 4294967298 3)

(section "logop")

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

(section "integer-length")

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

;; From Norman Hardy <norm@netcom.com>
(define (ssin x) (let ((a2 (quotient (* x x) dx)))
   (- x (let tail ((term x)(ndx 2))
      (let ((x (quotient (* a2 term) (* ndx (+ ndx 1) dx))))
         (if (zero? x) 0 (- x (tail x (+ ndx 2)))))))))
(define dx (expt 10 100))
(define pi
  31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679)
(test 3 ssin pi)

(test #f = (expt 2. 100) (+ (expt 2 100) 1))
(test #t = (expt 2. 100) (exact->inexact (+ (expt 2 100) 1)))

(test 2650239300 remainder 14853098170650239300 4000000000)

(test "8000000000000000" number->string #x8000000000000000 16)
(test "80000000000000000" number->string #x80000000000000000 16)
;; From Aubrey Jaffer <agj@alum.mit.edu>
(test #t number? (string->number "#i0/0+1i"))
(test #t number? (string->number "#i1+0/0i"))
(test #t positive? 2147483648)
(test #t negative? (string->number "#i-1/0"))

;; From Sven.Hartrumpf@fernuni-hagen.de
(define quotient-fix-1
  (lambda (a b x) (quotient (+ (quotient (* a x 10) b) 5) 10)))
(test 950 quotient-fix-1 95 100 1000)
;; Variations on Sven's test:
(define (quotient-fix-2 (a :: <real>))
  (quotient (+ a 20) 10))
(test 97 quotient-fix-2 950)
(define (quotient-float (a :: <real>))
  (quotient (+ a 25.0) 10))
(test 97.0 quotient-float 950)

(define v (vector 3 -4/5 9 10 (+ 8 1/2) -100))
(java.util.Collections:sort v)
(test #(-100 -4/5 3 17/2 9 10) 'sort-v-1 v)
(set! v (vector 1.2 -1.2 8.9 100.0 8.9))
(java.util.Collections:sort v)
(test #(-1.2 1.2 8.9 8.9 100.0) 'sort-v-2 v)
(set! v (vector 1 0.5 5/2 8 2.5))
(java.util.Collections:sort v)
(test #(0.5 1 5/2 2.5 8) 'sort-v-3 v)
