(define (char-numeric? char)
  ((primitive-static-method "java.lang.Character" "isDigit"
			     "boolean" ("char"))
   char))

(define (char-whitespace? char)
  ((primitive-static-method "java.lang.Character" "isSpace"
			     "boolean" ("char"))
   char))

(define (char-upper-case? char)
  ((primitive-static-method "java.lang.Character" "isUpperCase"
			     "boolean" ("char"))
   char))

(define (char-lower-case? char)
  ((primitive-static-method "java.lang.Character" "isLowerCase"
			     "boolean" ("char"))
   char))

(define (char->integer char)
  ((primitive-virtual-method "kawa.lang.Char" "intValue"
			     "int" ())
   char))

(define (integer->char n)
  ((primitive-static-method "kawa.lang.Char" "make"
			    "kawa.lang.Char" ("int"))
   n))

(define (char-upcase char)
  ((primitive-static-method "java.lang.Character" "toUpperCase"
			    "char" ("char"))
   char))

(define (char-downcase char)
  ((primitive-static-method "java.lang.Character" "toLowerCase"
			    "char" ("char"))
   char))
