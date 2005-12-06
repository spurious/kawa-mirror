(require <classes1>)

(define-class <ClsD> (<ClsB>)
  (d :: <int> init-form: 23))
;  ((f (y :: <int>)) :: <int> (+ d y)))

(define-class <ClsE> (<ClsC> <ClsD>)
  (e :: <int> init-form: 39)
  ((f (y :: <int>)) :: <int> (+ 100 xx  e y)))

(define-class-using-syntax-rules <SimpleC> <SimpleB>
  (d :: <int> init-form: 23 init-keyword: d:)
  (e :: <int> init-form: 24))

(define yy :: <int> 56)

(define date-test-instance (make <DateTest>))

(define-namespace date-test-ns <SimpleDateTest>)
(define (make-date-test)
  (let ((d :: <SimpleDateTest> (date-test-ns:new)))
    (date-test-ns:get-year d)))

(define-simple-class <TestCapture1> ()
  (z :: <integer> init: 11)
  ((ff farg)
   (list
    (lambda (y) (list yy z y)))))

(define-simple-class <TestCapture2> ()
  (z :: <integer> init: 12)
  ((ff farg)
   (list
    (lambda (y) (list yy y)))))

;; Test for Savannah bug #15151
(define (getClassTest o) (slot-ref o 'class))
