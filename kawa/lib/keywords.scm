(define (keyword? object)
  ((primitive-static-method <keyword> "isKeyword"
			    "boolean" (<object>))
   object))

(define (keyword->string keyword)
  ((primitive-constructor <string> (<String>))
   ((primitive-virtual-method <keyword> "getName" <String> ())
    keyword)))
;  ((primitive-virtual-method <keyword> "toSchemeString"
;			     <string> ())
;   keyword))

(define (string->keyword string)
  ((primitive-static-method <keyword> "make"
			    <keyword> ("String"))
   string))
