(require <classes1>)

(define-class <ClsD> (<ClsB>)
  (d :: <int> init-form: 23))
;  ((f (y :: <int>)) :: <int> (+ d y)))

(define-class <ClsE> (<ClsC> <ClsD>)
  (e :: <int> init-form: 39)
  ((f (y :: <int>)) :: <int> (+ 100 xx  e y)))

(define-simple-class <SimpleC> (<SimpleB>)
  (d :: <int> init-form: 23 init-keyword: d:)
  (e :: <int> init-form: 24))

(define yy :: <int> 56)

