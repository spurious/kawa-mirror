;; SYMBOLS

(define (symbolp x)
  (invoke-static <gnu.jemacs.lang.Symbol> 'isSymbol x))

(define (symbol-name symbol)
  (invoke-static <gnu.jemacs.lang.Symbol> 'getPrintName symbol))

;(define (make-symbol NAME) ...)

; (define (intern name #!optional obarray)  ...)

; (define (intern-soft NAME #!optonal obarray) ..)

;; obarray
;; mapatoms
;; unintern

(define (symbol-plist symbol)
  (invoke-static <gnu.jemacs.lang.Symbol> 'getPropertyList symbol))

(define (setplist symbol plist)
  (invoke-static <gnu.jemacs.lang.Symbol> 'setPropertyList symbol plist)
  plist)

(define (plist-get plist prop #!optional default)
  (invoke-static <gnu.jemacs.lang.Symbol> 'plistGet plist prop default))

(define (plist-put plist prop value)
  (invoke-static <gnu.jemacs.lang.Symbol> 'plistPut plist prop value))

(define (plist-remprop plist prop)
  (invoke-static <gnu.jemacs.lang.Symbol> 'plistRemove plist prop))

(define (plist-member plist prop)
  (if (eq
       (invoke-static <gnu.jemacs.lang.Symbol> 'plistGet plist prop #!void)
       #!void)
      '() 't))

(define (get symbol property #!optional (default '()))
  (plist-get (symbol-plist symbol) property default))

(define (put symbol property value)
  (setplist symbol (plist-put (symbol-plist symbol) property value)))

;; VARIABLES

;;A `void-variable' error is signaled if SYMBOL has neither a local
;;     binding nor a global value.

(define (symbol-value sym)
  (invoke (invoke-static <gnu.jemacs.lang.Symbol> 'getBinding sym) 'get))

;; setq

;(define (make-symbol NAME)  ...)

(define (set symbol value)
  (invoke-static <gnu.jemacs.lang.Symbol> 'setValueBinding symbol value))

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
  (invoke-static <gnu.jemacs.lang.Symbol> 'getFunctionBinding
		 symbol))

;(define (fboundp x) ..)

;(fmakunboud symbol)

(define (fset symbol object)
  (invoke-static <gnu.jemacs.lang.Symbol> 'setFunctionBinding
		 (invoke-static <gnu.mapping.Environment> 'getCurrent)
		 symbol object))

(define (make-local-variable symbol)
  (invoke-static <gnu.jemacs.lang.Symbol> 'makeBufferLocal symbol #f)
  symbol)
(define (make-variable-buffer-local symbol)
  (invoke-static <gnu.jemacs.lang.Symbol> 'makeBufferLocal symbol #t)
  symbol)
