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

;;  (symbol-plist)

;; VARIABLES

;;A `void-variable' error is signaled if SYMBOL has neither a local
;;     binding nor a global value.

(define (symbol-value sym)
  (invoke (invoke-static <gnu.jemacs.lang.Symbol> 'getBinding sym) 'get))

;; setq

;(define (make-symbol NAME)  ...)

; (define (intern name #!optional obarray)  ...)

; (define (intern-soft NAME #!optonal obarray)  ..)

;; obarray
;; mapatoms
;; unintern

;;  (symbol-plist)

;; VARIABLES

;;A `void-variable' error is signaled if SYMBOL has neither a local
;;     binding nor a global value.

;(define (symbol-value sym)
;  ...)

;; setq

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
