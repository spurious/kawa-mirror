(define (char? x)
  (instance? x <gnu.kawa.util.Char>))

(define (char-alphabetic? (char <char>))
  (invoke-static <java.lang.Character> "isLetter" char))

(define (char-numeric? (char <char>))
  (invoke-static <java.lang.Character> "isDigit" char))

(define (char-whitespace? (char <char>))
  (invoke-static <java.lang.Character> "isSpace" char))

(define (char-upper-case? (char <char>))
  (invoke-static <java.lang.Character> "isUpperCase" char))

(define (char-lower-case? (char <char>))
  (invoke-static <java.lang.Character> "isLowerCase" char))

(define (char->integer (char <char>))
  ((primitive-virtual-method <gnu.kawa.util.Char> "intValue"
			     <int> ())
   char))

(define (integer->char (n <int>))
  (invoke-static <gnu.kawa.util.Char> "make" n))

(define (char-upcase (char <char>))
  (invoke-static <java.lang.Character> "toUpperCase" char))

(define (char-downcase (char <char>))
  (invoke-static <java.lang.Character> "toLowerCase" char))
