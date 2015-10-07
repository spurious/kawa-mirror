(define-library (kawa base)
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
          $bracket-apply$
          $bracket-list$

          bitwise-not
          bitwise-and bitwise-ior bitwise-xor bitwise-if
          bitwise-bit-count bitwise-length bitwise-first-bit-set
          bitwise-bit-set? bitwise-copy-bit
          bitwise-bit-field bitwise-copy-bit-field
          bitwise-arithmetic-shift
          bitwise-arithmetic-shift-left
          bitwise-arithmetic-shift-right
          bitwise-rotate-bit-field
          bitwise-reverse-bit-field

          logop logtest
          logand logior logxor lognot logcount
          integer-length arithmetic-shift ash

          define-alias
	  define-variable

	  define-class
	  define-simple-class
          object
	  this

	  invoke
	  invoke-static
	  invoke-special
	  field
	  static-field
          set-field!
          set-static-field!
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

	  runnable future sleep
	  
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
   (rnrs arithmetic bitwise)
   (only (scheme r5rs) exact->inexact inexact->exact)
   (only gnu.kawa.lispexpr.BracketApply (instance $bracket-apply$))

   (only kawa.standard.syntax
         (quasiSyntax quasisyntax) syntax)

   (only (kawa lib std_syntax) identifier?)

   (only kawa.lib.syntax
         $bracket-list$
	 define-syntax-case
	 try-finally
	 synchronized)

   (only (kawa lib prim_syntax)
	 try-catch define-constant define-variable)

   (only (rename (only (kawa standard syntax_case) syntax_case)
		 (syntax_case syntax-case))
	 syntax-case)

   (only (kawa standard define_alias) (define_alias define-alias))

   (only (rename (kawa standard define_class)
		 (define_simple_class define-simple-class)
		 (define_class define-class))
	 define-simple-class define-class)

   (only (kawa standard object)
         (objectSyntax object))

   (only (rename (kawa standard thisRef)
		 (thisSyntax this))
	 this)

   (kawa reflect)

   (only (kawa lib numbers)
         logop logcount logtest (bitwise-length integer-length))

   (only (gnu kawa functions BitwiseOp)
         (and logand) (ior logior) (xor logxor) (not lognot)
         (ashift arithmetic-shift) (ashift ash))
   
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
	 future sleep runnable)

   ))
