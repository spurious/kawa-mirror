
(define (make-string n #!optional (ch #\Space))
  ((primitive-constructor <string> (<int> <char>))
   n ch))

(define (string-length str)
  ((primitive-virtual-method <string> "length" <int> ())
   str))
