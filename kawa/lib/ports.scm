(define (open-input-file (pathname :: <String>)) :: <input-port>
  (invoke-static <input-port> 'openFile pathname))

(define (open-output-file (pathname :: <String>)) :: <output-port>
  (invoke-static <output-port> 'openFile pathname))

(define (call-with-input-file (pathname :: <String>) (proc :: <procedure>))
 (let ((port :: <input-port> (open-input-file pathname)))
    (try-finally
     (proc port)
     (close-input-port port))))

(define (call-with-output-file pathname proc)
  (let ((port :: <output-port> (open-output-file pathname)))
    (try-finally
     (proc port)
     (close-output-port port))))

(define (with-input-from-file fname (proc :: <procedure>))
  (let ((port :: <input-port> (gnu.mapping.InPort:openFile
			       (invoke fname 'toString)))
	(save :: <input-port> (gnu.mapping.InPort:inDefault)))
    (try-finally
     (begin
       (gnu.mapping.InPort:setInDefault port)
       (proc))
     (begin
       (gnu.mapping.InPort:setInDefault save)
       (invoke port 'close)))))       

(define (with-output-to-file filename (proc :: <procedure>))
  (let* ((fname (invoke filename 'toString))
	 (port :: <output-port> (gnu.mapping.OutPort:new
				 (java.io.FileWriter:new fname)
				 fname))
	 (save :: <output-port> (gnu.mapping.OutPort:outDefault)))
    (try-finally
     (begin
       (gnu.mapping.OutPort:setOutDefault port)
       (proc))
     (begin
       (gnu.mapping.OutPort:setOutDefault save)
       (invoke port 'close)))))

(define (input-port? x) :: <boolean>
  (instance? x <input-port>))

(define (output-port? x) :: <boolean>
  (instance? x <output-port>))

(define-syntax define-alias-parameter
  (syntax-rules ()
    ((define-alias-parameter name type location)
     (begin
       (define-constant name :: <gnu.mapping.LocationProc>
	 (invoke-static <gnu.mapping.LocationProc> 'makeNamed
			(gnu.mapping.Symbol:make '|| 'name)
			location))
       (gnu.mapping.LocationProc:pushConverter
	name
	(lambda (arg)
	  (try-catch
	   (as type arg)
	   (ex <java.lang.ClassCastException>
	       (let ((wt (gnu.mapping.WrongType:make ex name
						     (as <int> 1) arg)))
		 (set! (field wt 'expectedType) type)
		 (primitive-throw wt))))))))))

(define-alias-parameter current-input-port <input-port>
  (static-field <input-port> 'inLocation))
(define-alias-parameter current-output-port <output-port>
  (static-field <output-port> 'outLocation))
(define-alias-parameter current-error-port <output-port>
  (static-field <output-port> 'errLocation))
	 
(define (write-char ch #!optional
		    (port :: <output-port>
			  (invoke-static  <output-port> 'outDefault)))
  :: <void>
  (invoke port 'writeChar (char->integer ch)))

;; SRFI-6
(define (open-input-string (str :: <string>)) :: <input-port>
  (make <gnu.mapping.CharArrayInPort>
    (field str 'data) (field str 'size)))

(define (open-output-string) :: <string-output-port>
  ((primitive-constructor  <string-output-port> ())))

(define (get-output-string (output-port  <string-output-port>))
  ((primitive-constructor <string> (<char[]>))
   ((primitive-virtual-method <string-output-port> "toCharArray"
                              <char[]> ())
    output-port)))

(define (call-with-input-string (str :: <string>) (proc :: <procedure>))
  (let* ((port :: <gnu.mapping.CharArrayInPort>
	  (make <gnu.mapping.CharArrayInPort>
	    (field str 'data) (field str 'size)))
	 (result (proc port)))
    (close-input-port port)
    result))

(define (call-with-output-string (proc :: <procedure>))
  (let ((port :: <gnu.mapping.CharArrayOutPort>
	      (make <gnu.mapping.CharArrayOutPort>)))
    (proc port)
    (let ((chars :: <char[]> (invoke port 'toCharArray)))
      (invoke port 'close)
      (make <string> chars))))

(define (force-output #!optional (port (current-output-port)))
  ((primitive-virtual-method <java.io.Writer> "flush" <void> ())
   port))

(define (newline #!optional (port (current-output-port)))
  ((primitive-virtual-method <output-port> "println"
			     <void> ())
   port))

(define (eof-object? obj)
  (eq? obj #!eof))

(define (char-ready? #!optional (port (current-input-port)))
  (invoke-static <kawa.standard.char_ready_p> 'ready port))

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

