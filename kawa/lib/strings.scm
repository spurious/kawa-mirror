
(define (make-string n #!optional (ch #\Space))
  ((primitive-constructor "kawa.lang.FString" ("int" "char"))
   n ch))

(define (string-length str)
  ((primitive-virtual-method "kawa.lang.FString" "length"
			     "int" ())
   str))
