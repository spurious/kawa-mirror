(define (input-port? x)
  (instance? x <input-port>))

(define (output-port? x)
  (instance? x <output-port>))

(define (call-with-input-string str proc)
  (let* ((port
	  ((primitive-virtual-method <string> "open" <input-port> ())
	   str))
	 (result (proc port)))
    (close-input-port port)
    result))

(define (force-output #!optional (port (current-output-port)))
  ((primitive-virtual-method <java.io.Writer> "flush" <void> ())
   port))

(define (newline #!optional (port (current-output-port)))
  ((primitive-virtual-method <output-port> "println"
			     <void> ())
   port))

(define (eof-object? obj)
  (eq? obj #!eof))

(define (input-port-read-state port)
  ((primitive-virtual-method <input-port> "getReadState" <char> ())
   port))

(define (port-line #!optional (port (current-input-port)))
  ((primitive-virtual-method <kawa.lang.LineBufferedReader> "getLineNumber"
			     <int> ())
   port))
(define (input-port-line-number port)
  (+ 1 (port-line port)))

(define (set-port-line! port line)
  ((primitive-virtual-method <kawa.lang.LineBufferedReader> "setLineNumber"
			     <void> (<int>))
   port line))

(define (set-input-port-line-number! port num)
  (set-port-line port (- num 1)))

(define (port-column #!optional (port (current-input-port)))
  ((primitive-virtual-method <kawa.lang.LineBufferedReader>
			     "getColumnNumber" <int> ())
   port))

(define (input-port-column-number port)
  (+ 1 (port-column port)))

(define (default-prompter port)
  (let ((state (input-port-read-state port)))
    (if (char=? state #\Newline)
	""
	(string-append (if (char=? state #\Space)
			   "#|kawa:"
			   (string-append "#|" (make-string 1 state) "---:"))
		       (number->string (input-port-line-number port))
		       "|# "))))

(define (input-port-prompter port)
  ((primitive-virtual-method <kawa.lang.TtyInPort> "getPrompter"
			       <kawa.lang.Procedure> ())
   port))

(define (set-input-port-prompter! port prompter)
  ((primitive-virtual-method <kawa.lang.TtyInPort> "setPrompter"
			       <void> (<kawa.lang.Procedure>))
   port prompter))

(define (transcript-on filename)
  ((primitive-static-method <output-port> "setLogFile" <void>
			    (<java.lang.String>))
   filename))

(define (transcript-off)
  ((primitive-static-method <output-port> "closeLogFile" <void> ())))
