;;; DSSSL unit-declaration
(define-syntax define-unit
  (syntax-rules ()
		((define-syntax unit-name expression)
		 (begin
		   ((primitive-static-method "gnu.math.Unit" "define"
					     "gnu.math.Unit"
					     ("String" "gnu.math.Quantity"))
		    'unit-name expression)
		   #!void))))
