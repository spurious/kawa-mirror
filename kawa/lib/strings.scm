
(define (make-string n #!optional (ch #\Space))
  ((primitive-constructor <string> (<int> <char>))
   n ch))

(define (string-length str)
  ((primitive-virtual-method <string> "length" <int> ())
   str))

(define (substring str start end)
  ((primitive-virtual-method <string> "copy" <string> (<int> <int>))
   str start end))

(define (string-copy! str)
  ((primitive-virtual-method <string> "copy" <string> ())
   str))

(define (string-fill! str ch)
  ((primitive-virtual-method <string> "fill" <void> (<char>))
   str ch))
