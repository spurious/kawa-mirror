(define-constant xx :: <int> 20)

(define-simple-class <SimpleA> ()
  (a :: <int> init-value: 4)
  (b :: <int> init-form: 6 allocation: class:)
  ((f (y :: <int>)) :: <int> (+ xx a b y)))

(define-class <ClsB> ()
  (b :: <int> 14))

(define-class <ClsC> (<ClsB>)
  (c :: <int> init-form: 22)
  ((f (y :: <int>)) :: <int> (+ xx c y)))
