(define (force-output #!optional (port (current-output-port)))
  ((primitive-virtual-method "java.io.OutputStream" "flush" "void" ())
   port))

(define (newline #!optional (port (current-output-port)))
  ((primitive-virtual-method "kawa.lang.OutPort" "println"
			     "void" ())
   port))

(define (eof-object? obj)
  (eq? obj #!eof))
