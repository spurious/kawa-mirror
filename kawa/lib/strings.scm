(module-export string? make-string $make$string$ string-length
               string-ref string-set!
               char=? char<? char>? char<=? char>=?
               char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
               string=? string<? string>? string<=? string>=?
	       string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
               substring string->list list->string string-copy string-copy!
               string-fill! string-upcase! string-downcase!
               string-capitalize string-capitalize!
               string-append string-append/shared)

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.lists>)

(define-syntax define-compare
  (syntax-rules ()
    ((_ NAME TYPE OP COMP2)
     (define (NAME (str1 ::TYPE) (str2 ::TYPE)
                          #!rest (strs ::TYPE[]))
       ::boolean
       (and (OP (COMP2 str1 str2) 0)
            (let ((n ::int strs:length))
              (let loop ((i ::int 0) (prev ::TYPE str2))
                (or (= i n)
                    (let ((next (strs i)))
                      (and (OP (COMP2 prev next) 0)
                           (loop (+ i 1) next)))))))))))

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

(define (string-ref (string ::java.lang.CharSequence) (k ::int)) ::char
  (invoke string 'charAt k))

(define (string-set! string::abstract-string k::int char::char)
  ::void
  (invoke string 'setCharAt k char))

(define (%string-compare2 (str1 :: string) (str2 :: string)) ::int
  ((str1:toString):compareTo (str2:toString)))

(define-compare string<? string < %string-compare2)
(define-compare string=? string = %string-compare2)
(define-compare string>? string > %string-compare2)
(define-compare string<=? string <= %string-compare2)
(define-compare string>=? string >= %string-compare2)

(define (substring (str <string>) (start <int>) (end <int>))
  :: <string>
  (make <gnu.lists.FString> str start (- end start)))

(define (string->list (str ::string)
                      #!optional (start ::int 0) (end ::int (str:length)))
  ::list
  (let loop ((result ::list '())
	     (i ::int end))
    (set! i (- i 1))
    (if (< i start)
	result
	(loop (make <pair> (string-ref str i) result) i))))

(define (list->string (lst ::list)) ::string
  (let* ((len ::int (length lst))
	 (result ::string (make gnu.lists.FString len)))
    (do ((i ::int 0 (+ i 1)))
	((>= i len) result)
      (let ((pair ::pair lst))
	(string-set! result i pair:car)
	(set! lst pair:cdr)))))

(define (string-copy (str ::java.lang.CharSequence)
                     #!optional
                     (start ::int 0)
                     (end ::int (str:length)))
  ::gnu.lists.FString
  (gnu.lists.FString str start (- end start)))

(define (string-copy! (to ::abstract-string)
                      (at ::int)
                      (from ::java.lang.CharSequence)
                      #!optional
                      (start ::int 0)
                      (end ::int (from:length)))
  ::void
  (gnu.lists.Strings:copyInto from start end to at))

(define (string-fill! str ::abstract-string ch ::char
                      #!optional
                      (start ::int 0)
                      (end ::int (str:length)))
  ::void
  (str:fill start end ch))

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

(define (%string-compare-ci2 (str1 :: string) (str2 :: string)) ::int
  (invoke (gnu.kawa.functions.UnicodeUtils:foldCase str1)
          'compareTo
          (gnu.kawa.functions.UnicodeUtils:foldCase str2)))

(define-compare string-ci<? string < %string-compare-ci2)
(define-compare string-ci=? string = %string-compare-ci2)
(define-compare string-ci>? string > %string-compare-ci2)
(define-compare string-ci<=? string <= %string-compare-ci2)
(define-compare string-ci>=? string >= %string-compare-ci2)

(define (%char-compare (c1 :: character) (c2 :: character)) ::int
  (- (invoke c1 'intValue) (invoke c2 'intValue)))

(define-compare char=? character = %char-compare)
(define-compare char<? character < %char-compare)
(define-compare char>? character > %char-compare)
(define-compare char<=? character <= %char-compare)
(define-compare char>=? character >= %char-compare)

(define (%char-compare-ci (c1 :: character) (c2 :: character)) ::int
  (- (java.lang.Character:toUpperCase (c1:intValue))
     (java.lang.Character:toUpperCase (c2:intValue))))

(define-compare char-ci=? character = %char-compare-ci)
(define-compare char-ci<? character < %char-compare-ci)
(define-compare char-ci>? character > %char-compare-ci)
(define-compare char-ci<=? character <= %char-compare-ci)
(define-compare char-ci>=? character >= %char-compare-ci)
