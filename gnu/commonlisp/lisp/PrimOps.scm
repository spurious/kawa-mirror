;;; For now these are written in Scheme.
;;; They should be re-written in Common Lisp, but there are still some
;;; limitations in the Common Lisp supprt making that difficult.

(define (car x)
  (if (eq? x '()) x (slot-ref (as <pair> x) 'car)))

(define (cdr x)
  (if (eq? x '()) x (slot-ref (as <pair> x) 'cdr)))

(define (setcar (p <pair>) x)
  (set-car! p x))

(define (setcdr (p <pair>) x)
  (set-cdr! p x))

;; SYMBOLS

(define (boundp symbol) :: <clisp:boolean>
  ((primitive-static-method <gnu.commonlisp.lang.Symbols> "isBound"
                            <boolean> (<object>))
   symbol))

(define (symbolp x) :: <clisp:boolean>
  (invoke-static <gnu.commonlisp.lang.Symbols> 'isSymbol x))

(define (symbol-name symbol)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'getPrintName symbol))

;(define (make-symbol NAME) ...)

; (define (intern name #!optional obarray)  ...)

; (define (intern-soft NAME #!optonal obarray) ..)

;; obarray
;; mapatoms
;; unintern

;;(define (symbol-plist symbol)
;;  (invoke-static <gnu.commonlisp.lang.Symbols> 'getPropertyList symbol))

;;(define (setplist symbol plist)
;;  (invoke-static <gnu.commonlisp.lang.Symbols> 'setPropertyList symbol plist)
;;  plist)

(define (plist-get plist prop #!optional default)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'plistGet plist prop default))

(define (plist-put plist prop value)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'plistPut plist prop value))

(define (plist-remprop plist prop)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'plistRemove plist prop))

(define (plist-member plist prop)
  (if (eq?
       (invoke-static <gnu.commonlisp.lang.Symbols> 'plistGet plist prop #!void)
       #!void)
      '() 't))

(define (get (symbol :: <gnu.mapping.Binding>) property #!optional (default '()))
  (invoke symbol 'getProperty property default))

(define (put (symbol :: <gnu.mapping.Binding>) property value)
  (invoke symbol 'setProperty property value))

;; VARIABLES

;;A `void-variable' error is signaled if SYMBOL has neither a local
;;     binding nor a global value.

(define (symbol-value sym)
  (invoke (invoke-static <gnu.commonlisp.lang.Symbols> 'getBinding sym) 'get))

;; setq

;(define (make-symbol NAME)  ...)

(define (set symbol value)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'setValueBinding symbol value))

#|
(define (add-to-list symbol value)
  (let ((old (symbol-value symbol)))
    (or (elisp::member value old) ;; FIXME
	(set symbol (cons value (symbol-value symbol))))))
|#

;; FUNCTIONS

;; This returns the object in the function cell of SYMBOL.  If the
;; symbol's function cell is void, a `void-function' error is signaled.

(define (symbol-function symbol)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'getFunctionBinding
		 symbol))

;(define (fboundp x) ..)

;(fmakunboud symbol)

(define (fset symbol object)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'setFunctionBinding
		 (invoke-static <gnu.mapping.Environment> 'getCurrent)
		 symbol object))

(define (apply func #!rest (args :: <Object[]>))
  (invoke-static 'gnu.kawa.functions.Apply 'applyN
		 (if (symbol? func) (symbol-function func) func)
		 args))

(define-syntax prog1
  (syntax-rules ()
		((prog1 first)
		 first)
		((prog1 first rest ...)
		 (let ((%prog1-save% first)) ;; Should be lexical-let? FIXME
		   (begin rest ...)
		   %prog1-save%))))

;;; ARRAYS

(define (length (x :: <gnu.lists.Sequence>))
  (invoke x 'size))

(define (arrayp x) <clisp:boolean>
  (instance? x <gnu.lists.SimpleVector>))

(define (aref
         (array <gnu.lists.SimpleVector>)
         (k <int>))
  (invoke array 'get k))

(define (aset (array <gnu.lists.SimpleVector>)
	      (k <int>)
	      obj)
  (invoke array 'set k obj)
  obj)

(define (fillarray (array <gnu.lists.SimpleVector>) obj)
  (invoke array 'fill obj)
  obj)

;;; STRINGS

(define (stringp x) <clisp:boolean>
  (instance? x <string>))

(define (make-string (count :: <int>) ch)
  (make <string> count (invoke-static <gnu.commonlisp.lang.CommonLisp> 'asChar ch)))

(define (substring (str :: <string>) from #!optional (to '()))
  (if (eq? to '())
      (set! to (string-length str)))
  (if (< to 0)
      (set! to (- (string-length str) to)))
  (if (< from 0)
      (set! from (- (string-length str) from)))
  (invoke str 'copy from to))

(define (char-to-string ch)
  (make <string> 1 (invoke-static <gnu.commonlisp.lang.CommonLisp> 'asChar ch)))
