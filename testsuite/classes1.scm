(define-constant xx :: <int> 20)

(define-simple-class <SimpleA> ()
  (a :: <int> init-value: 4)
  (b :: <int> init-form: 6 allocation: class:)
  (hyphenated-field? init-value: "yes")
  ((f (y :: <int>)) :: <int>
   (if (equal? hyphenated-field? "yes") (+ xx a b y) 999)))

(define-class <ClsB> ()
  (b :: <int> 14))

(define-class <ClsC> (<ClsB>)
  (c :: <int> init-form: 22)
  ((f (y :: <int>)) :: <int> (+ xx c y)))
