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
