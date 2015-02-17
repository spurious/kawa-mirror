(define-library (kawa reflect)
  (export invoke
	  invoke-static
	  invoke-special
	  field
	  static-field
	  make
	  instance?
	  as
	  primitive-throw
	  )
  (import
   (only (rename (gnu kawa reflect Invoke)
		 (invokeStatic invoke-static)
		 (invokeSpecial invoke-special))
	 invoke
	 invoke-static
	 invoke-special
	 make)

   (only (rename (gnu kawa reflect SlotGet)
		 (staticField static-field))
	 field
	 static-field)

   (only (rename (gnu kawa reflect Throw)
		 (primitiveThrow primitive-throw))
	 primitive-throw)

   (only (rename (kawa standard Scheme)
		 (instanceOf instance?))
	 instance?)

   (only (gnu kawa functions Convert)
	 as)
   ))
