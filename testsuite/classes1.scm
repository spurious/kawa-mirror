(define-constant xx :: <int> 20)

(define-simple-class <SimpleA> ()
  (a :: <int> init-value: 4)
  (b :: <int> init-form: 6 allocation: class:)
  (hyphenated-field? init-value: "yes")
  ((lambda-method1)
   (lambda (x) (make-vector 1 x))) ; This lambda doesn't "capture" anything.
  ((lambda-method2 n)
   (lambda (x) (make-vector (+ a n) x))) ; This lambda does.
  ((f (y :: <int>)) :: <int>
   (if (equal? hyphenated-field? "yes") (+ xx a b y) 999)))

(define-class <ClsB> ()
  (b :: <int> 14))

(define-class <ClsC> (<ClsB>)
  (c :: <int> init-form: 22)
  ((f (y :: <int>)) :: <int> (+ xx c y)))

(define-syntax define-class-using-syntax-rules
  (syntax-rules ()
		((define-class-using-syntax-rules name super parts ...)
		 (define-simple-class name (super) parts ...))))

(define-simple-class <SimpleDateTest> (<java.util.Date>)
  ((get-year) :: <int>
   (+ (invoke-special <java.util.Date> (this) 'get-year) 1900)))

