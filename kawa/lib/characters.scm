(define (char? x)
  (instance? x <kawa.lang.Char>))

(define (char-alphabetic? (char <char>))
  ((primitive-static-method <java.lang.Character> "isLetter"
			     <boolean> (<char>))
   char))

(define (char-numeric? (char <char>))
  ((primitive-static-method <java.lang.Character> "isDigit"
			     <boolean> (<char>))
   char))

(define (char-whitespace? (char <char>))
  ((primitive-static-method <java.lang.Character> "isSpace"
			     <boolean> (<char>))
   char))

(define (char-upper-case? (char <char>))
  ((primitive-static-method <java.lang.Character> "isUpperCase"
			     <boolean> (<char>))
   char))

(define (char-lower-case? (char <char>))
  ((primitive-static-method <java.lang.Character> "isLowerCase"
			     <boolean> (<char>))
   char))

(define (char->integer (char <char>))
  ((primitive-virtual-method <kawa.lang.Char> "intValue"
			     <int> ())
   char))

(define (integer->char (n <int>))
  ((primitive-static-method <kawa.lang.Char> "make"
			    <kawa.lang.Char> (<int>))
   n))

(define (char-upcase (char <char>))
  ((primitive-static-method <java.lang.Character> "toUpperCase"
			    <char> (<char>))
   char))

(define (char-downcase (char <char>))
  ((primitive-static-method <java.lang.Character> "toLowerCase"
			    <char> (<char>))
   char))
