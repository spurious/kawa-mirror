(define (string? x)
  (instance? x <string>))

(define (string=? x y)
  (invoke (invoke x 'toString) 'equals (invoke y 'toString)))

(define (make-string (n :: <int>) #!optional (ch #\Space))
  (make <string> n ch))

(define (string-length (str :: <abstract-string>))
  (invoke str 'length))

(define (string-ref (string :: <abstract-string>) (k :: <int>))
  (invoke string 'charAt k))

(define (string-set! (string :: <abstract-string>) (k <int>) (char <char>))
  (invoke string 'setCharAt k char))

(define (substring (str <string>) (start <int>) (end <int>))
  ((primitive-virtual-method <string> "copy" <string> (<int> <int>))
   str start end))

(define (string-copy (str <abstract-string>))
  (invoke str 'copy))

(define (string-fill! (str <abstract-string>) (ch <char>))
  (invoke str 'fill ch))

(define (string-upcase! (str :: <abstract-string>))
  (invoke str 'makeUpperCase)
  str)

(define (string-downcase! (str :: <abstract-string>))
  (invoke str 'makeLowerCase)
  str)

(define (string-capitalize! (str :: <abstract-string>))
  (invoke str 'makeCapitalize)
  str)

(define (string-upcase (str :: <abstract-string>))
  (string-upcase! (string-copy str)))

(define (string-downcase (str :: <abstract-string>))
  (string-downcase! (string-copy str)))

(define (string-capitalize (str :: <abstract-string>))
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
