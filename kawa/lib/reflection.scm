;;; RECORDS

(define (make-record-type name fnames)
  ((primitive-static-method "kawa.lang.Record" "makeRecordType"
			    "java.lang.Class" ("String" "kawa.lang.List"))
   name fnames))

(define (record-constructor cl #!optional (flds (record-type-field-names cl)))
  ((primitive-constructor "kawa.lang.RecordConstructor"
			  ("java.lang.Class" "Object"))
   cl flds))

(define (record-accessor class fname)
  ((primitive-constructor "kawa.lang.GetFieldProc"
			  ("java.lang.Class" "String"))
   class fname))

(define (record-modifier class fname)
  ((primitive-constructor "kawa.lang.SetFieldProc"
			  ("java.lang.Class" "String"))
   class fname))

(define (record? obj)
  ((primitive-static-method "kawa.lang.Record" "isRecord"
			    "boolean" ("Object"))
   obj))

(define (record-predicate class)
  (lambda (object)
    ((primitive-virtual-method "java.lang.Class" "isAssignableFrom"
			       "boolean" ("java.lang.Class"))
     class
     ((primitive-virtual-method "java.lang.Object" "getClass"
				"java.lang.Class" ())
      object))))

(define (record-type-descriptor object)
  ((primitive-virtual-method "java.lang.Object" "getClass"
			     "java.lang.Class" ())
   object))

(define (record-type-name rtd)
  ((primitive-constructor "kawa.lang.FString" ("String"))
   ((primitive-virtual-method "java.lang.Class" "getName"
			      "String" ())
    rtd)))

(define (record-type-field-names rtd)
  ((primitive-static-method "kawa.lang.Record" "typeFieldNames"
			   "kawa.lang.List" ("java.lang.Class"))
   rtd))

#| THESE CAN BE FUNCTIONS WHEN WE HAVE BETTER INLINING:
(define (primitive-array-new element-type)
  ((primitive-constructor <kawa.lang.PrimArrayNew> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-set element-type)
  ((primitive-constructor <kawa.lang.PrimArraySet> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-get element-type)
  ((primitive-constructor <kawa.lang.PrimArrayGet> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-length element-type)
  ((primitive-constructor <kawa.lang.PrimArrayLength> (<gnu.bytecode.Type>))
   element-type))
... etc ...
|#
(define-syntax primitive-array-new
  (syntax-rules ()
		((primitive-array-new element-type)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.PrimArrayNew> (<gnu.bytecode.Type>))
		  element-type))))
(define-syntax primitive-array-set
  (syntax-rules ()
		((primitive-array-set element-type)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.PrimArraySet> (<gnu.bytecode.Type>))
		  element-type))))
(define-syntax primitive-array-get
  (syntax-rules ()
		((primitive-array-get element-type)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.PrimArrayGet> (<gnu.bytecode.Type>))
		  element-type))))
(define-syntax primitive-array-length
  (syntax-rules ()
		((primitive-array-length element-type)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.PrimArrayLength> (<gnu.bytecode.Type>))
		  element-type))))

(define-syntax primitive-get-field
  (syntax-rules ()
		((primitive-get-field ctype fname ftype)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.GetFieldProc>
		   (<gnu.bytecode.ClassType> <String> <gnu.bytecode.Type>
					     <int>))
		  ctype fname ftype 1 #|PUBLIC|#))))
(define-syntax primitive-set-field
  (syntax-rules ()
		((primitive-put-field ctype fname ftype)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.SetFieldProc>
		   (<gnu.bytecode.ClassType> <String> <gnu.bytecode.Type>
					     <int>))
		  ctype fname ftype 1 #|PUBLIC|#))))

(define-syntax primitive-get-static
  (syntax-rules ()
		((primitive-get-field ctype fname ftype)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.PrimGetStatic>
		   (<gnu.bytecode.ClassType> <String> <gnu.bytecode.Type>
					     <int>))
		  ctype fname ftype 9 #|PUBLIC|STATIC|#))))
(define-syntax primitive-set-static
  (syntax-rules ()
		((primitive-put-field ctype fname ftype)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.PrimSetStatic>
		   (<gnu.bytecode.ClassType> <String> <gnu.bytecode.Type>
					     <int>))
		  ctype fname ftype 9 #|PUBLIC|STATIC|#))))
