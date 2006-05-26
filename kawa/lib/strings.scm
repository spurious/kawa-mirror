(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.lists>)

(define (string? x) :: <boolean>
  (instance? x <string>))

(define (make-string (n :: <int>) #!optional (ch #\Space)) :: <string>
  (make <string> n ch))

(define (string #!rest (args :: <Object[]>)) :: <string>
  (let* ((n :: <int> args:length)
	 (str (<gnu.lists.FString> n)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n) str)
	(str:setCharAt i ((as <character> (args i)):charValue)))))

(define (string-length (str :: <abstract-string>)) :: <int>
  (invoke str 'length))

(define (string-ref (string :: <abstract-string>) (k :: <int>)) :: <char>
  (invoke string 'charAt k))

(define (string-set! (string :: <abstract-string>) (k <int>) (char <char>))
  :: <void>
  (invoke string 'setCharAt k char))

(define (string=? x y) :: <boolean>
  (invoke (invoke x 'toString) 'equals (invoke y 'toString)))

(define (string-ci=? x y) :: <boolean>
  (invoke (invoke x 'toString) 'equalsIgnoreCase (invoke y 'toString)))

(define (string<? x y) :: <boolean>
  (< (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (string>? x y) :: <boolean>
  (> (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (string<=? x y) :: <boolean>
  (<= (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (string>=? x y) :: <boolean>
  (>= (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (string-ci<? x y) :: <boolean>
  (< (invoke (invoke (invoke x 'toString) 'toLowerCase)
	     'compareTo
	     (invoke (invoke y 'toString) 'toLowerCase))
     0))

(define (string-ci>? x y) :: <boolean>
  (> (invoke (invoke (invoke x 'toString) 'toLowerCase)
	     'compareTo
	     (invoke (invoke y 'toString) 'toLowerCase))
     0))

(define (string-ci<=? x y) :: <boolean>
  (<= (invoke (invoke (invoke x 'toString) 'toLowerCase)
	      'compareTo
	      (invoke (invoke y 'toString) 'toLowerCase))
      0))

(define (string-ci>=? x y) :: <boolean>
  (>= (invoke (invoke (invoke x 'toString) 'toLowerCase)
	      'compareTo
	      (invoke (invoke y 'toString) 'toLowerCase))
      0))

(define (substring (str <abstract-string>) (start <int>) (end <int>))
  :: <string>
  (make <string> str start (- end start)))

(define (string->list (str :: <abstract-string>)) :: <list>
  (let loop ((result :: <list> '())
	     (i :: <int> (string-length str)))
    (set! i (- i 1))
    (if (< i 0)
	result
	(loop (make <pair> (string-ref str i) result) i))))

(define (list->string (list :: <list>)) :: <string>
  (let* ((len :: <int> (length list))
	 (result :: <string> (make <string> len)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i len) result)
      (let ((pair :: <pair> list))
	(string-set! result i (field pair 'car))
	(set! list (field pair 'cdr))))))

(define (string-copy (str <abstract-string>)) :: <string>
  (make <string> str))

(define (string-fill! (str <abstract-string>) (ch <char>)) :: <void>
  (invoke str 'fill ch))

(define (string-upcase! (str :: <abstract-string>)) :: <abstract-string>
  (invoke-static <gnu.lists.Strings> 'makeUpperCase str)
  str)

(define (string-downcase! (str :: <abstract-string>)) :: <abstract-string>
  (invoke-static <gnu.lists.Strings> 'makeLowerCase str)
  str)

(define (string-capitalize! (str :: <abstract-string>)) :: <abstract-string>
  (invoke-static <gnu.lists.Strings> 'makeCapitalize str)
  str)

(define (string-upcase (str :: <abstract-string>)) :: <string> 
  (let ((copy :: <string> (string-copy str)))
    (invoke-static <gnu.lists.Strings> 'makeUpperCase copy)
    copy))

(define (string-downcase (str :: <abstract-string>)) :: <string> 
  (let ((copy :: <string> (string-copy str)))
    (invoke-static <gnu.lists.Strings> 'makeLowerCase copy)
    copy))

(define (string-capitalize (str :: <abstract-string>)) :: <string> 
  (let ((copy :: <string> (string-copy str)))
    (invoke-static <gnu.lists.Strings> 'makeCapitalize copy)
    copy))

(define (string-append #!rest (args :: <Object[]>)) :: <string>
  (let ((str :: <string> (make <string>)))
    (invoke str 'addAllStrings args 0)
    str))

(define (string-append/shared #!rest (args :: <Object[]>)) :: <string>
  (if (= 0 args:length)
      (make <string>)
      (let ((arg1 :: <string> (args 0)))
        (invoke arg1 'addAllStrings args 1)
        arg1)))
