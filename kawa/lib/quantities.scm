;;; DSSSL unit-declaration
(define-syntax define-unit
  (syntax-rules ()
		((define-syntax unit-name expression)
		 ((primitive-static-method "kawa.math.Unit" "define"
					   "kawa.math.Unit"
					   ("String" "kawa.math.Quantity"))
		  'unit-name expression))))
