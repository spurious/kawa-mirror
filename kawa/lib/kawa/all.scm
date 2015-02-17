(define-library (kawa all)
  (include-library-declarations
   "../scheme/base-exports"
   "../scheme/char-exports"
   "../scheme/complex-exports"
   "../scheme/cxr-exports"
   "../scheme/eval-exports"
   "../scheme/file-exports"
   "../scheme/inexact-exports"
   "../scheme/lazy-exports"
   "../scheme/process-context-exports"
   "../scheme/time-exports"
   "../scheme/write-exports"
   )
  (export case-lambda load read interaction-environment
          exact->inexact inexact->exact)
  (export syntax
	  quasisyntax
	  syntax-case
	  define-syntax-case
	  identifier?

	  define-alias
	  define-variable

	  define-class
	  define-simple-class
	  this

	  invoke
	  invoke-static
	  invoke-special
	  field
	  static-field
	  make
	  instance?
	  as

	  primitive-throw
	  try-finally
	  try-catch
	  synchronized

	  call-with-input-string
	  call-with-output-string
	  force-output
	  format

	  make-process
	  command-parse

	  runnable
	  
	  scheme-implementation-version
	  reverse!
	  )
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme char)
   (scheme complex)
   (scheme cxr)
   (scheme eval)
   (scheme file)
   (scheme inexact)
   (scheme lazy)
   (scheme load)
   (scheme process-context)
   (scheme read)
   (scheme repl)
   (scheme time)
   (scheme write)
   (only (scheme r5rs) exact->inexact inexact->exact)

   (only (rename (kawa standard syntax)
		 (quasiSyntax quasisyntax))
	 syntax quasisyntax)

   (only (kawa lib std_syntax) identifier?)

   (only (kawa lib syntax)
	 define-syntax-case
	 try-finally
	 synchronized)

   (only (kawa lib prim_syntax)
	 try-catch)

   (only (rename (only (kawa standard syntax_case) syntax_case)
		 (syntax_case syntax-case))
	 syntax-case)

   (only (rename (kawa standard define_alias)
		 (define_alias define-alias))
	 define-alias)

   (only (rename (kawa standard define_variable)
		 (define_variable define-variable))
	 define-variable)

   (only (rename (kawa standard define_class)
		 (define_simple_class define-simple-class)
		 (define_class define-class))
	 define-simple-class define-class)

   (only (rename (kawa standard thisRef)
		 (thisSyntax this))
	 this)

   (kawa reflect)
   
   (only (kawa lib ports)
	 call-with-input-string
	 call-with-output-string
	 force-output)

   (only (gnu kawa functions Format)
	 format)

   (only (kawa lib misc)
	 scheme-implementation-version)

   (only (kawa lib lists)
	 reverse!)

   (only (kawa lib system)
	 make-process command-parse)

   (only (kawa lib thread)
	 runnable)

   ))
