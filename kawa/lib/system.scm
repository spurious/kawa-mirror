(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.vectors>)
(require <kawa.lib.exceptions>)

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
  (let* ((count :: <int> (length lst))
	 (arr ((primitive-array-new <String>) count)))
    (let loop ((p lst) (i :: <int> 0))
      (if (null? p) arr
	  (let ((pp :: <pair> p))
	    ((primitive-array-set <String>) arr i pp:car)
	    (loop pp:cdr (+ i 1)))))))

(define (tokenize-string-to-string-array (string :: <String>))
  (let* ((toks (make <java.util.StringTokenizer> string))
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
	 (count :: <int> (length rlist))
	 (arr ((primitive-array-new <String>) count)))
    (let loop ((p rlist) (i :: <int> (- count 1)))
      (if (null? p) arr
	  (let ((pp :: <pair> p))
	    ((primitive-array-set <String>) arr i  pp:car)
	    (loop pp:cdr (- i 1)))))))

(define (tokenize-string-using-shell string)
  (let ((arr :: <java.lang.String[]>
             ((primitive-array-new <java.lang.String>) 3)))
    ((primitive-array-set <String>) arr 0 "/bin/sh")
    ((primitive-array-set <String>) arr 1 "-c")
    ((primitive-array-set <String>) arr 2 string)
    arr))

(define command-parse :: <function>
  (if (equal? (java.lang.System:getProperty "file.separator") "/")
      tokenize-string-using-shell
      tokenize-string-to-string-array))

(define (compile-file (source :: <string>)
		      (output :: <String>))
  :: <void>
  (let* ((messages :: <gnu.text.SourceMessages>
		   (make <gnu.text.SourceMessages>))
	 (comp :: <gnu.expr.Compilation>
	       (invoke-static <kawa.lang.CompileFile> 'read (source:toString) messages)))
    (set! comp:explicit #t)
    (if (invoke messages 'seenErrors)
	(primitive-throw (make <gnu.text.SyntaxException> messages)))
    (invoke comp 'compileToArchive
	    (invoke comp 'getModule)
	    output)
    (if (invoke messages 'seenErrors)
	(primitive-throw (make <gnu.text.SyntaxException> messages)))))

(define (process-command-line-assignments)
  (gnu.expr.ApplicationMainSupport:processSetProperties))

(define (get-environment-variable name::string)
  (let ((r (java.lang.System:getenv (name:toString))))
    (if (eq? r #!null) #f r)))

(define (get-environment-variables)
  (let ((it (((java.lang.System:getenv):entrySet):iterator)))
    (let loop ((r '()))
      (if (it:hasNext)
          (let ((e (it:next)))
            (loop (cons (cons (e:getKey) (e:getValue)) r)))
          r))))

(define (current-second) ::double
  (* (java.lang.System:currentTimeMillis) 0.001))

(define (current-jiffy) ::long
  (java.lang.System:nanoTime))

(define (jiffies-per-second) ::long
  1000000000)

(define-simple-constructor cmd run-process $string-with-delimiter-marks$)
(define-simple-constructor sh run-process-using-sh $string-with-delimiter-marks$)
(define-syntax run-process-using-sh
  (syntax-rules ()
    ((run-process-using-sh . args)
     (run-process shell: #t . args))))

(define-syntax pipe-process
  (syntax-rules ()
    ((_ e0) e0)
    ((_ e0 e1 . rest)
     (pipe-process (%pipe-process e0 e1) . rest))))

(define (pipeProcessValidateApply
         exp::gnu.expr.ApplyExp
         visitor::gnu.expr.InlineCalls
         required::gnu.bytecode.Type
         proc::gnu.mapping.Procedure) ::gnu.expr.Expression
         (exp:visitArgs visitor)
         (cond ((gnu.expr.ErrorExp? exp)
                exp)
               ((= exp:arg-count 2)
                (let ((e0 (exp:getArg 0))
                      (e1 (exp:getArg 1)))
                  (if (and (gnu.expr.ApplyExp? e1)
                           (eq? ((->gnu.expr.ApplyExp e1):function:valueIfConstant)
                                gnu.kawa.functions.RunProcess:instance))
                      (let* ((ae1 ::gnu.expr.ApplyExp e1)
                             (aeargs ae1:args)
                             (xargs (gnu.expr.Expression[]
                                                        length: (+ 2 aeargs:length))))
                        (set! (xargs 0) (gnu.expr.QuoteExp:getInstance 'in:))
                        (set! (xargs 1) e0)
                        (java.lang.System:arraycopy aeargs 0 xargs 2 aeargs:length)
                        (visitor:visitApplyOnly
                         (gnu.expr.ApplyExp ae1:function xargs)
                         required))
                      (visitor:error #\e "pipe-process arg not run-process" e1))))
               (else
                (visitor:error "pipe-process - internal error - expected 2 args"))))

(define-procedure %pipe-process
  validate-apply: "kawa.lib.system:pipeProcessValidateApply"
  (lambda (e1 e2)
    (java.lang.RuntimeException "%pipe-process called")))

(define (process-exit-wait process::java.lang.Process) ::int
  ((->java.lang.Process process):waitFor))

(define (process-exit-ok? process::java.lang.Process) ::boolean
  (= ((->java.lang.Process process):waitFor) 0))
