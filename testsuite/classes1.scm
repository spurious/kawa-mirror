(module-static counter get-new-count)

(define-constant xx :: <int> 20)

(define-simple-class <SimpleA> ()
  (a :: <int> init: (- b 2))
  (b :: <int> init-form: 6 allocation: class:)
  (hyphenated-field? init-value: "yes")
  ((lambda-method1)
   (lambda (x) (make-vector 1 x))) ; This lambda doesn't "capture" anything.
  ((lambda-method2 n)
   (lambda (x) (make-vector (+ a n) x))) ; This lambda does.
  ((f (y :: <int>)) :: <int>
   (if (equal? hyphenated-field? "yes") (+ xx a b y) 999)))

(define-class <ClsB> ()
  (b :: <int> 14)) ;; deprecated syntax

(define-class <ClsC> (<ClsB>)
  (c :: <int>)
  (:init (set! c 22))
  ((f (y :: <int>)) :: <int> (+ xx c y)))

(define-syntax define-class-using-syntax-rules
  (syntax-rules ()
		((define-class-using-syntax-rules name super parts ...)
		 (define-simple-class name (super) parts ...))))

(define *MY-YEAR-OFFSET* 1900)
(define (default-offset)
  *MY-YEAR-OFFSET*)

(define-class <DateTest> (<java.util.Date>)
  (offset init-form:  (default-offset))
  ((get-year) :: <int>
   (+ (invoke-special <java.util.Date> (this) 'get-year) offset)))

(define-simple-class <SimpleDateTest> (<java.util.Date>)
  ((get-year) :: <int>
   (+ (invoke-special <java.util.Date> (this) 'get-year) 1900)))

(define-private counter :: <int> 0)

(define (get-new-count)
  (set! counter (+ counter 1))
  counter)

(define-simple-class <IdClass1> ()
  (var0 allocation: 'class init: (get-new-count))
  (var1 init-form: (get-new-count)))
 
(define-simple-class <IdClass2> (<IdClass1>)
  (allocation: 'class init: (get-new-count))
  (var2 init-form: (get-new-count)))
