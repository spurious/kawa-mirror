(define (string? x)
  (instance? x <string>))

(define (string=? x y)
  ((primitive-virtual-method <object> "equals" <boolean> (<object>))
   ((primitive-virtual-method <object> "toString" <String> ()) x)
   ((primitive-virtual-method <object> "toString" <String> ()) y)))

(define (make-string n #!optional (ch #\Space))
  ((primitive-constructor <string> (<int> <char>))
   n ch))

(define (string-length str)
  ((primitive-virtual-method <string> "length" <int> ())
   str))

(define (string-ref string (k <int>))
  ((primitive-virtual-method <string> "charAt" <char> (<int>))
   string k))

(define (string-set! string (k <int>) (char <char>))
  ((primitive-virtual-method <string> "setCharAt" <void> (<int> <char>))
   string k char))

(define (substring str (start <int>) (end <int>))
  ((primitive-virtual-method <string> "copy" <string> (<int> <int>))
   str start end))

(define (string-copy str)
  ((primitive-virtual-method <string> "copy" <string> ())
   str))

(define (string-fill! str (ch <char>))
  ((primitive-virtual-method <string> "fill" <void> (<char>))
   str ch))

(define (string-upcase! str)
  ((primitive-virtual-method <string> "makeUpperCase" <void> ())
   str)
  str)

(define (string-downcase! str)
  ((primitive-virtual-method <string> "makeLowerCase" <void> ())
   str)
  str)

(define (string-capitalize! str)
  ((primitive-virtual-method <string> "makeCapitalize" <void> ())
   str)
  str)

(define (string-upcase str)
  (string-upcase! (string-copy str)))

(define (string-downcase str)
  (string-downcase! (string-copy str)))

(define (string-capitalize str)
  (string-capitalize! (string-copy str)))
