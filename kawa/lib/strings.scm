(define (string? x)
  (instance? x <string>))

(define (string=? x y)
  ((primitive-virtual-method <object> "equals" <boolean> (<object>))
   ((primitive-virtual-method <object> "toString" <String> ()) x)
   ((primitive-virtual-method <object> "toString" <String> ()) y)))

(define (make-string n #!optional (ch #\Space))
  (make <string> n ch))

(define (string-length (str <string>))
  ((primitive-virtual-method <string> "length" <int> ())
   str))

(define (string-ref (string <string>) (k <int>))
  ((primitive-virtual-method <string> "charAt" <char> (<int>))
   string k))

(define (string-set! (string <string>) (k <int>) (char <char>))
  ((primitive-virtual-method <string> "setCharAt" <void> (<int> <char>))
   string k char))

(define (substring (str <string>) (start <int>) (end <int>))
  ((primitive-virtual-method <string> "copy" <string> (<int> <int>))
   str start end))

(define (string-copy (str <string>))
  ((primitive-virtual-method <string> "copy" <string> ())
   str))

(define (string-fill! (str <string>) (ch <char>))
  ((primitive-virtual-method <string> "fill" <void> (<char>))
   str ch))

(define (string-upcase! (str <string>))
  ((primitive-virtual-method <string> "makeUpperCase" <void> ())
   str)
  str)

(define (string-downcase! (str <string>))
  ((primitive-virtual-method <string> "makeLowerCase" <void> ())
   str)
  str)

(define (string-capitalize! (str <string>))
  ((primitive-virtual-method <string> "makeCapitalize" <void> ())
   str)
  str)

(define (string-upcase (str <string>))
  (string-upcase! (string-copy str)))

(define (string-downcase (str <string>))
  (string-downcase! (string-copy str)))

(define (string-capitalize (str <string>))
  (string-capitalize! (string-copy str)))

#|
(define (string->list (str <string>))
  (let* ((len :: <int>
	      ((primitive-virtual-method <string> "length" <int> ()) str))
	 (result '()))
    (do ((i :: <int> (- len (as <int> 1)) (- i (as <int> 1))))
	((>= i (as <int> 0))
	 result)
      (set! result ((primitive-constructor <pair> (<object> <object>))
		    ((primitive-static-method <character> "make" <character>
					      (<char>))
		       ((primitive-virtual-function <string> "charAt"
						    <char> (<int>))
			str i))
		    result)))))
|#
