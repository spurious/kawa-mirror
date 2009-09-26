(module-static #t)

(define-namespace Repl "class:kawa.repl")
(define-namespace Repl2 <kawa.repl>)

(define (set-home1 (x :: <String>)) (set! (<kawa.repl>:.homeDirectory) x))
(define (set-home2 (x :: <String>)) (set! <kawa.repl>:homeDirectory x))
(define (set-home3 (x :: <String>)) (set! (Repl:.homeDirectory) x))
(define (set-home4 (x :: <String>)) (set! Repl:homeDirectory x))
(define (set-home5 (x :: <String>)) (set! (kawa.repl:.homeDirectory) x))
(define (set-home6 (x :: <String>)) (set! kawa.repl:homeDirectory x))
(define (set-home7 (x :: <String>)) (set! (Repl2:.homeDirectory) x))
(define (set-home8 (x :: <String>)) (set! Repl2:homeDirectory x))

(define (get-home1) (<kawa.repl>:.homeDirectory))
(define (get-home2) <kawa.repl>:homeDirectory)
(define (get-home3) (Repl:.homeDirectory))
(define (get-home4) Repl:homeDirectory)
(define (get-home5) (kawa.repl:.homeDirectory))
(define (get-home6) kawa.repl:homeDirectory)
(define (get-home7) (Repl2:.homeDirectory))
(define (get-home8) Repl2:homeDirectory)

(define-namespace Pair1 <pair>)
(define-namespace Pair2 <gnu.lists.Pair>)
(define-namespace Pair3 "class:gnu.lists.Pair")

(define (set-car1 (p <pair>) x)  (set! (*:.car p) x))
(define (set-car2 (p <pair>) x)  (set! (<pair>:.car p) x))
(define (set-car3 (p <pair>) x)  (set! (gnu.lists.Pair:.car p) x))
(define (set-car4 (p <pair>) x)  (set! (<gnu.lists.Pair>:.car p) x))
(define (set-car5 (p <pair>) x)  (set! (Pair1:.car p) x))
(define (set-car6 (p <pair>) x)  (set! (Pair2:.car p) x))
(define (set-car7 (p <pair>) x)  (set! (Pair3:.car p) x))
(define (set-car8 (p <pair>) x)  (set!  p:car x))
(define (set-car9 p x)  (set! (Pair1:.car p) x))

(define (get-car1 (p <pair>))  (*:.car p))
(define (get-car2 (p <pair>))  (<pair>:.car p))
(define (get-car3 (p <pair>))  (gnu.lists.Pair:.car p))
(define (get-car4 (p <pair>))  (<gnu.lists.Pair>:.car p))
(define (get-car5 (p <pair>))  (Pair1:.car p))
(define (get-car6 (p <pair>))  (Pair2:.car p))
(define (get-car7 (p <pair>))  (Pair3:.car p))
(define (get-car8 (p <pair>))  p:car)
(define (get-car9 p)  (Pair3:.car p))
(define (get-car10 p)  (<pair>:.car p))

(define (is-pair1 x) (<pair>:instance? x))
(define (is-pair2 x) (gnu.lists.Pair:instance? x))
(define (is-pair3 x) (<gnu.lists.Pair>:instance? x))
(define (is-pair4 x) (Pair1:instance? x))
(define (is-pair5 x) (Pair2:instance? x))
(define (is-pair6 x) (Pair3:instance? x))
(define (is-pair7 x) (instance? x <pair>))
(define (is-pair9 x) (instance? x <gnu.lists.Pair>))
(define (is-pair10 x) (instance? x Pair1:<>))
(define (is-pair11 x) (instance? x Pair2:<>))
(define (is-pair12 x) (instance? x Pair3:<>))
(define (is-pair13 x) (gnu.lists.Pair? x))
(define (is-pair14 x) (Pair1? x))
(define (is-pair15 x) (Pair2? x))
(define (is-pair16 x) (Pair3? x))

(define (cast-to-pair1 x) (<pair>:@ x))
(define (cast-to-pair2 x) (gnu.lists.Pair:@ x))
(define (cast-to-pair3 x) (<gnu.lists.Pair>:@ x))
(define (cast-to-pair4 x) (Pair1:@ x))
(define (cast-to-pair5 x) (Pair2:@ x))
(define (cast-to-pair6 x) (Pair3:@ x))
(define (cast-to-pair7 x) (as <pair> x))
(define (cast-to-pair9 x) (as <gnu.lists.Pair> x))
(define (cast-to-pair10 x) (as Pair1:<> x))
(define (cast-to-pair11 x) (as Pair2:<> x))
(define (cast-to-pair12 x) (as Pair3:<> x))

(define (new-pair1 x y) (<pair>:new x y))
(define (new-pair2 x y) (gnu.lists.Pair:new x y))
(define (new-pair3 x y) (<gnu.lists.Pair>:new x y))
(define (new-pair4 x y) (Pair1:new x y))
(define (new-pair5 x y) (Pair2:new x y))
(define (new-pair6 x y) (Pair3:new x y))
(define (new-pair7 x y) (make <pair> x y))
(define (new-pair9 x y) (make <gnu.lists.Pair> x y))
(define (new-pair10 x y) (make Pair1:<> x y))
(define (new-pair11 x y) (make Pair2:<> x y))
(define (new-pair12 x y) (make Pair3:<> x y))

(define (is-empty1 (p <pair>)) ;; OK
  (*:isEmpty p))

(define (make-iarr1 (n :: <int>)) (make <int[]> size: n))
(define (make-iarr2 (n :: <int>)) (<int[]> size: n))
(define (make-iarr3 (n :: <int>)) (<int[]> size: n 3 4 5))

(define (length-iarr1 (arr :: <int[]>)) :: <int>
  (field arr 'length))
(define (length-iarr2(arr :: <int[]>)) :: <int>
  (*:.length arr))
(define (length-iarr3 (arr :: <int[]>)) :: <int>
  arr:length)
(define (get-iarr1 (arr :: <int[]>) (i :: <int>)) :: <int>
  (arr i))

(define (set-iarr1 (arr :: <int[]>) (i :: <int>) (val :: <int>)) :: <void>
  (set! (arr i) val))

#|
(define (car1 (x :: <pair>)) ;; OK
  (*:.car x))
(define (get-ns str)
  (<gnu.mapping.Namespace>:getInstance str))

(define (xcarx (p <pair>))
  (p:isEmpty))
;  (*:isEmpty p))
;  (*:.car p))
;(define-alias xx #,(namespace "XX"))
(define TWO xx:TWO)
|#
(define-simple-class <Int> ()
  (value :: <int>)
  ((toHex)
   (<java.lang.Integer>:toHexString value))
  ((toHex x) allocation: 'static
   (<java.lang.Integer>:toHexString x))
  ((toHex x) allocation: 'static
   (<java.lang.Integer>:toHexString x)))
(define (tohex1 x)
  (<Int>:toHex x))
(define (tohex2 (x :: <Int>))
  (invoke x 'toHex))
(define (tohex3 (x :: <Int>))
  (x:toHex))

(define (varargs1)
  (invoke gnu.math.IntNum 'getMethod "valueOf"
 java.lang.String java.lang.Integer:TYPE))
(define (varargs2 (argtypes :: java.lang.Class[]))
  (invoke gnu.math.IntNum 'getMethod "valueOf" argtypes))
(define (varargs3 argtypes)
  (invoke gnu.math.IntNum 'getMethod "valueOf" argtypes))

(define-namespace xx "XX")
(define xx:two 222)
(define list-two (list 'xx:Two))

(define (factoriali1 x :: int) :: int
  (if (< x 1) 1
      (* x (factoriali1 (- x 1)))))
(define (factoriali2 x :: <int>) :: <int>
  (if (< x 1) 1
      (* x (factoriali2 (- x 1)))))
(define (factoriall1 x :: long) :: long
  (if (< x 1) 1
      (* x (factoriall1 (- x 1)))))
(define (factorialI1 x :: <integer>) :: <integer>
  (if (< x 1) 1
      (* x (factorialI1 (- x 1)))))

(define (plus-lambda1) :: int
  ((lambda (x y) (+ x y)) 3 4))

(define (first-negative (vals :: double[])) :: double
  (let ((count vals:length))
    (call-with-current-continuation
     (lambda (exit)
       (do ((i :: int 0 (+ i 1)))
	   ((= i count)
	    0)
	   (let ((x (vals i)))
	     (if (< x 0)
		 (exit x))))))))

(define (inline-two-calls (x :: int)) :: int
  (define (f (w :: int)) (+ w 10))
  (if (> x 0)
      (let ((y1 (+ x 1)))
	(f y1))
      (let ((y2 (+ x 2)))
	(f y2))))

(define (inline-two-functions x)
  (letrec ((f (lambda ()
                (if x (f) (g))))
           (g (lambda ()
                (if x (g) (f)))))
    (f)))

(define (check-even (x :: int))
  (letrec ((even?
	    (lambda ((n1 :: int))
	      (if (= n1 0)
		  #t
		  (odd? (- n1 1)))))
	   (odd?
	    (lambda ((n2 :: int))
	      (if (= n2 0)
		  #f
		  (even? (- n2 1))))))
    (even? x)))
