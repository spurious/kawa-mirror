(define (input-port? x)
  (instance? x <input-port>))

(define (output-port? x)
  (instance? x <output-port>))

(define (current-input-port)
  ((primitive-static-method <input-port> "inDefault" <input-port> ())))

(define (current-output-port)
  ((primitive-static-method <output-port> "outDefault" <output-port> ())))

;; SRFI-6
(define (open-input-string string)
  ((primitive-virtual-method <string> "open" <input-port> ())
   string))

(define (open-output-string) <string-output-port>
  ((primitive-constructor  <string-output-port> ())))

(define (get-output-string (output-port  <string-output-port>))
  ((primitive-constructor <string> (<char[]>))
   ((primitive-virtual-method <string-output-port> "toCharArray"
                              <char[]> ())
    output-port)))

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

(define (port-line port)
  ((primitive-virtual-method <gnu.text.LineBufferedReader> "getLineNumber"
			     <int> ())
   port))
(define (input-port-line-number port)
  (+ 1 (port-line port)))

(define (set-port-line! port line)
  ((primitive-virtual-method <gnu.text.LineBufferedReader> "setLineNumber"
			     <void> (<int>))
   port line))

(define (set-input-port-line-number! port num)
  (set-port-line port (- num 1)))

(define (port-column port)
  ((primitive-virtual-method <gnu.text.LineBufferedReader>
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
  ((primitive-virtual-method <gnu.mapping.TtyInPort> "getPrompter"
			       <gnu.mapping.Procedure> ())
   port))

(define (set-input-port-prompter! port prompter)
  ((primitive-virtual-method <gnu.mapping.TtyInPort> "setPrompter"
			       <void> (<gnu.mapping.Procedure>))
   port prompter))

(define (close-input-port (port :: <input-port>))
  (invoke port 'close))

(define (close-output-port (port :: <output-port>))
  (invoke port 'close))

(define (transcript-on filename)
  ((primitive-static-method <output-port> "setLogFile" <void>
			    (<String>))
   filename))

(define (transcript-off)
  ((primitive-static-method <output-port> "closeLogFile" <void> ())))
