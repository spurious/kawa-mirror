(define (keyword? object)
  ((primitive-static-method "kawa.lang.Keyword" "isKeyword"
			    "boolean" ("Object"))
   object))

(define (keyword->string keyword)
  ((primitive-static-method "kawa.lang.Keyword" "toSchemeString"
			    "kawa.lang.FString" ("kawa.lang.Keyword"))
   keyword))

(define (string->keyword string)
  ((primitive-static-method "kawa.lang.Keyword" "make"
			    "kawa.lang.Keyword" ("String"))
   string))
