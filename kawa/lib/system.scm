(define (make-process args env)
  (let* ((arargs
	  (cond ((vector? args) (convert-vector-to-string-array args))
		((list? args) (convert-list-to-string-array args))
		((string? args) (command-parse args))
		((instance? args <java.lang.String[]>) args)
		(#t (error "invalid arguments to make-process"))))
	 (runtime ((primitive-static-method <java.lang.Runtime> "getRuntime"
					    <java.lang.Runtime> ())))
	 (process ((primitive-virtual-method
		    <java.lang.Runtime> "exec" <java.lang.Process>
		    (<java.lang.String[]> <java.lang.String[]>))
		   runtime arargs env)))
    process))

(define (open-input-pipe command)
  ((primitive-virtual-method <java.lang.Process> "getInputStream"
			       <java.io.InputStream> ())
   (make-process command #!null)))

;; (define (close-pipe port) ...  )

(define (system command)
  ((primitive-virtual-method <java.lang.Process> "waitFor" <int> ())
   (make-process command #!null)))

;; These are in Guile:
;; (define (open-input-pipe command) ... (make-process command #!null) ...)
;; (define (open-output-pipe command) ... (make-process command #!null) ...)

(define (convert-vector-to-string-array vec)
  (let* ((count (vector-length vec))
	 (arr ((primitive-array-new <java.lang.String>) count)))
    (do ((i 0 (+ i 1)))
	((= i count) arr)
      ((primitive-array-set <String>) arr i (vector-ref vec i)))))

(define (convert-list-to-string-array lst)
  (let* ((count (length lst))
	 (arr ((primitive-array-new <String>) count)))
    (do ((p lst (cdr p))
	 (i 0 (+ i 1)))
	((null? p) arr)
      ((primitive-array-set <String>) arr i (car p)))))


(define (tokenize-string-to-string-array string)
  (let* ((toks ((primitive-constructor <java.util.StringTokenizer> (<String>))
		string))
	 (rlist
	  (do ((list '() (cons
			  ((primitive-virtual-method
			    <java.util.StringTokenizer> "nextToken"
			    <java.lang.String> ())
			   toks) list)))
	      ((not ((primitive-virtual-method <java.util.StringTokenizer>
					       "hasMoreTokens" <boolean> ())
		     toks))
	       list)
	    #!void))
	 (count (length rlist))
	 (arr ((primitive-array-new <String>) count)))
    (do ((p rlist (cdr p))
	 (i (- count 1) (- i 1)))
	((null? p) arr)
      ((primitive-array-set <String>) arr i (car p)))))

(define (tokenize-string-using-shell string)
  (let ((arr :: <java.lang.String[]>
             ((primitive-array-new <java.lang.String>) 3)))
    ((primitive-array-set <String>) arr 0 "/bin/sh")
    ((primitive-array-set <String>) arr 1 "-c")
    ((primitive-array-set <String>) arr 2 string)
    arr))
