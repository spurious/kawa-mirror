(define (emacs:read #!optional (port (current-input-port)))
  (let ((lexer
	 ((primitive-constructor <gnu.elisp.ELispReader> (<input-port>)) port)))
    ((primitive-virtual-method <gnu.elisp.ELispReader> "readObject" <object> ())
     lexer)))

