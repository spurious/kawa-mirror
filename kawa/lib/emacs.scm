;; Support for Emacs Lisp.
;; This will be moved to some other package, but for now there
;; are messy order-dependencies (gnu.* is made before kawa.lib).

(define (emacs:read #!optional (port (current-input-port)))
  (let ((lexer
	 ((primitive-constructor <gnu.jemacs.lang.ELispReader> (<input-port>))
	  port)))
    ((primitive-virtual-method <gnu.jemacs.lang.ELispReader> "readObject"
			       <object> ())
     lexer)))

(define (emacs:symbol-name (symbol <gnu.mapping.Binding>))
  ((primitive-static-method <gnu.jemacs.lang.ELisp> "getString" <object> (<gnu.mapping.Binding>))
   symbol))

(define (boundp symbol)
  ((primitive-static-method <gnu.jemacs.lang.Symbol> "isBound"
                            <boolean> (<object>))
   symbol))
