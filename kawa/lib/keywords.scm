(define (keyword? object)
  ((primitive-static-method <keyword> "isKeyword"
			    "boolean" (<object>))
   object))

(define (keyword->string keyword)
  ((primitive-virtual-method <keyword> "toSchemeString"
			     <string> ())
   keyword))

(define (string->keyword string)
  ((primitive-static-method <keyword> "make"
			    <keyword> ("String"))
   string))
