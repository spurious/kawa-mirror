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
  :: <void>
  (invoke string 'setCharAt k char))

(define (substring (str <abstract-string>) (start <int>) (end <int>))
  (make <string> str start (- end start)))

(define (string->list (str :: <abstract-string>)) :: <list>
  (let loop ((result :: <list> '())
	     (i :: <int> (string-length str)))
    (set! i (- i 1))
    (if (< i 0)
	result
	(loop (cons (string-ref str i) result) i))))

(define (list->string (list :: <list>)) :: <string>
  (let* ((len :: <int> (length list))
	 (result :: <string> (make <string> len)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i len) result)
      (let ((pair :: <pair> list))
	(string-set! result i (car pair))
	(set! list (cdr pair))))))

(define (string-copy (str <abstract-string>))
  (make <string> str))

(define (string-fill! (str <abstract-string>) (ch <char>))
  (invoke str 'fill ch))

(define (string-upcase! (str :: <abstract-string>))
  (invoke-static <gnu.lists.Strings> 'makeUpperCase str)
  str)

(define (string-downcase! (str :: <abstract-string>))
  (invoke-static <gnu.lists.Strings> 'makeLowerCase str)
  str)

(define (string-capitalize! (str :: <abstract-string>))
  (invoke-static <gnu.lists.Strings> 'makeCapitalize str)
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
