(define (input-port? x)
  (instance? x <input-port>))

(define (output-port? x)
  (instance? x <output-port>))

(define (current-input-port)
  ((primitive-static-method <input-port> "inDefault" <input-port> ())))

(define (current-output-port)
  ((primitive-static-method <output-port> "outDefault" <output-port> ())))

(define (current-error-port)
  ((primitive-static-method <output-port> "errDefault" <output-port> ())))

(define (write-char ch #!optional
		    (port :: <output-port>
			  (invoke-static  <output-port> 'outDefault)))
  (invoke port 'writeChar (char->integer ch)))

;; SRFI-6
(define (open-input-string (str :: <string>)) :: <input-port>
  (make <gnu.mapping.CharArrayInPort>
    (field str 'data) (field str 'size)))

(define (open-output-string) <string-output-port>
  ((primitive-constructor  <string-output-port> ())))

(define (get-output-string (output-port  <string-output-port>))
  ((primitive-constructor <string> (<char[]>))
   ((primitive-virtual-method <string-output-port> "toCharArray"
                              <char[]> ())
    output-port)))

(define (call-with-input-string (str :: <string>) proc)
  (let* ((port
	  (make <gnu.mapping.CharArrayInPort>
	    (field str 'data) (field str 'size)))
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

(define (set-port-line! port line)
  ((primitive-virtual-method <gnu.text.LineBufferedReader> "setLineNumber"
			     <void> (<int>))
   port line))

(define-procedure port-line
  setter: set-port-line!
  (begin
    (define (port-line (port :: <gnu.text.LineBufferedReader>))
      (invoke port 'getLineNumber))
    port-line))

(define (set-input-port-line-number! port num)
  (set-port-line! port (- num 1)))

(define-procedure input-port-line-number
  setter: set-input-port-line-number!
  (begin
    (define (input-port-line-number (port :: <gnu.text.LineBufferedReader>))
      (+ 1 (port-line port)))
    input-port-line-number))

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

(define (set-input-port-prompter!
	 (port :: <gnu.mapping.TtyInPort>) (prompter :: <procedure>))
  (invoke port 'setPrompter prompter))

(define-procedure input-port-prompter
  setter: set-input-port-prompter!
  (begin
    (define (input-port-prompter (port :: <gnu.mapping.TtyInPort>))
      (invoke port 'getPrompter))
    input-port-prompter))

(define (close-input-port (port :: <input-port>))
  (invoke port 'close))

(define (close-output-port (port :: <output-port>))
  (invoke port 'close))

(define (transcript-on filename) :: <void>
  (invoke-static <output-port> 'setLogFile (invoke filename 'toString)))

(define (transcript-off)
  (invoke-static <output-port> 'closeLogFile))

