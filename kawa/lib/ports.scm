(define (force-output #!optional (port (current-output-port)))
  ((primitive-virtual-method <java.io.Writer> "flush" <void> ())
   port))

(define (newline #!optional (port (current-output-port)))
  ((primitive-virtual-method <kawa.lang.OutPort> "println"
			     <void> ())
   port))

(define (eof-object? obj)
  (eq? obj #!eof))

(define (input-port-read-state port)
  ((primitive-virtual-method <kawa.lang.InPort> "getReadState" <char> ())
   port))

(define (input-port-line-number port)
  (+ 1
     ((primitive-virtual-method <kawa.lang.LineBufferedReader> "getLineNumber"
				<int> ())
      port)))

(define (set-input-port-line-number! port num)
  ((primitive-virtual-method <kawa.lang.LineBufferedReader> "setLineNumber"
			     <void> (<int>))
   port (- num 1)))

(define (input-port-column-number port)
  ((+ 1
      ((primitive-virtual-method <kawa.lang.LineBufferedReader>
				 "getColumnNumber" <int> ())
       port))))

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
  ((primitive-static-method <kawa.lang.OutPort> "setLogFile" <void>
			    (<java.lang.String>))
   filename))

(define (transcript-off)
  ((primitive-static-method <kawa.lang.OutPort> "closeLogFile" <void> ())))
