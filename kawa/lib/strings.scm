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

(define (substring str start end)
  ((primitive-virtual-method <string> "copy" <string> (<int> <int>))
   str start end))

(define (string-copy str)
  ((primitive-virtual-method <string> "copy" <string> ())
   str))

(define (string-fill! str ch)
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
