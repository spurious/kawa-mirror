(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.lists>)

(define (string? x) :: <boolean>
  (instance? x <string>))

(define (make-string n ::int #!optional (ch #\Space)) :: <string>
  (make <gnu.lists.FString> n ch))

(define ($make$string$ #!rest args ::Object[]) :: <string>
  (let* ((n :: <int> args:length)
	 (str (<gnu.lists.FString> n)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n) str)
	(str:setCharAt i ((as <character> (args i)):charValue)))))

(define (string-length str ::string) :: <int>
  (invoke str 'length))

(define (string-ref (string :: <string>) (k :: <int>)) :: <char>
  (invoke string 'charAt k))

(define (string-set! string::abstract-string k::int char::char)
  ::void
  (invoke string 'setCharAt k char))

(define (string=? x y) ::boolean
  ((x:toString):equals (y:toString)))

(define (string<? x y) ::boolean
  (< (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (string>? x y) :: <boolean>
  (> (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (string<=? x y) :: <boolean>
  (<= (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (string>=? x y) :: <boolean>
  (>= (invoke (invoke x 'toString) 'compareTo (invoke y 'toString)) 0))

(define (substring (str <string>) (start <int>) (end <int>))
  :: <string>
  (make <gnu.lists.FString> str start (- end start)))

(define (string->list (str :: <string>)) :: <list>
  (let loop ((result :: <list> '())
	     (i :: <int> (string-length str)))
    (set! i (- i 1))
    (if (< i 0)
	result
	(loop (make <pair> (string-ref str i) result) i))))

(define (list->string (list :: <list>)) :: <string>
  (let* ((len :: <int> (length list))
	 (result :: <string> (make <gnu.lists.FString> len)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i len) result)
      (let ((pair :: <pair> list))
	(string-set! result i pair:car)
	(set! list pair:cdr)))))

(define (string-copy (str <string>)) :: <gnu.lists.FString>
  (make <gnu.lists.FString> str))

(define (string-fill! str ::abstract-string ch ::char) ::void
  (str:fill ch))

(define (string-upcase! str ::abstract-string) ::string
  (gnu.lists.Strings:makeUpperCase str)
  str)

(define (string-downcase! (str :: <abstract-string>)) :: <string>
  (invoke-static <gnu.lists.Strings> 'makeLowerCase str)
  str)

(define (string-capitalize! (str :: <abstract-string>)) :: <string>
  (invoke-static <gnu.lists.Strings> 'makeCapitalize str)
  str)

(define (string-capitalize (str :: <string>)) :: <string> 
  (let ((copy :: <gnu.lists.FString> (string-copy str)))
    (invoke-static <gnu.lists.Strings> 'makeCapitalize copy)
    copy))

(define (string-append #!rest (args :: <Object[]>)) :: <gnu.lists.FString>
  (let ((str :: <gnu.lists.FString> (make <gnu.lists.FString>)))
    (invoke str 'addAllStrings args 0)
    str))

(define (string-append/shared #!rest (args :: <Object[]>)) :: <string>
  (if (= 0 args:length)
      (make <gnu.lists.FString>)
      (let ((arg1 (args 0)))
	(let ((fstr :: <gnu.lists.FString>
		    (if (instance? arg1 <gnu.lists.FString>) arg1
			(string-copy arg1))))
	  (invoke fstr 'addAllStrings args 1)
	  fstr))))
