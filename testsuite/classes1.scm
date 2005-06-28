(module-static counter get-new-count call-lambda)

(define-constant xx :: <int> 20)

(define-simple-class <SimpleA> ()
  (two :: <int> init-form: 2 access: 'private)
  (a :: <int> init: (- b two))
  (b :: <int> init-form: 6 allocation: class:)
  (n22 :: <int> init-value: 22 allocation: 'static access: 'protected)
  (hyphenated-field? init-value: "yes")

  (mHappy init: #t)
  ((isHappy) :: <boolean>
   mHappy)
  ((setHappy val :: <boolean>) :: <void>
   (set! mHappy val))

  ((lambda-method1)
   (lambda (x) (make-vector 1 x))) ; This lambda doesn't "capture" anything.
  ((lambda-method2 n)
   (lambda (x) (make-vector (+ a n) x))) ; This lambda does.
  ((lambda-method3)
   (call-lambda (lambda () (slot-ref (this) 'two))))
  ((lambda-method4)
   (call-lambda (lambda () two)))
  ((lambda-method5 x)
   (lambda () (set! two x)))
  ((lambda-method6)
   (lambda (x) (set! two x)))
  ((lambda-method7)
   (lambda (x) (slot-set! (this) 'two x)))
  ((lambda-method-rest1 name)
   (lambda (arg1 . rest) (list name arg1 rest)))
  ((x1900) :: <int> access: 'package allocation: 'static
   1900)
  ((g) :: <int> access: 'protected
   (+ xx a))
  ((f (y :: <int>)) :: <int>
   (if (equal? hyphenated-field? "yes") (+ (g) b y) 999))

  ;; Bug reported by Dean Ferreyra <dferreyra@igc.org> 2005-06-09
  ((trouble)
   (let ((fn (lambda (o)
               (get-identity-property-name o))))
     fn))
  ((get-identity-property-name property-name)
   (string-append property-name "Identity")))

(define (call-lambda fn)
  (fn))

(define-class <ClsB> ()
  (b :: <int> 14)) ;; deprecated syntax

(define-class <ClsC> (<ClsB>)
  (c :: <int>)
  (:init (set! c (static-field <SimpleA> 'n22)))
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
   (+ (invoke-special <java.util.Date> (this) 'get-year)
      (invoke-static <SimpleA> 'x1900))))

(define-simple-class <IdClass1> ()
  (var0 allocation: 'class init: (get-new-count))
  (var1 init-form: (get-new-count)))

(define-private counter :: <int> 0)

(define (get-new-count)
  (set! counter (+ counter 1))
  counter)

(define-simple-class <IdClass2> (<IdClass1>)
  (allocation: 'class init: (get-new-count))
  (var2 init-form: (get-new-count)))

(define-namespace date-test-ns <SimpleDateTest>)
(define (make-date-test)
  (let ((d :: <SimpleDateTest> (date-test-ns:new)))
    (date-test-ns:get-year d)))
