;;; RECORDS

(define (make-record-type name fnames)
  ((primitive-static-method <record> "makeRecordType"
			    <class-type> (<String> <list>))
   name fnames))

(define (record-constructor cl #!optional (flds #!null))
  ((primitive-constructor <kawa.lang.RecordConstructor>
			  (<class-type> <object>))
   cl flds))

(define (record-accessor class fname)
  ((primitive-constructor "kawa.lang.GetFieldProc" (<class-type> "String"))
   class fname))

(define (record-modifier class fname)
  ((primitive-constructor <kawa.lang.SetFieldProc> (<class-type> "String"))
   class fname))

(define (record? obj)
  (instance? obj <record>))

(define (record-predicate rtype)
  (lambda (object)
    ((primitive-virtual-method <type> "isInstance" <boolean> (<object>))
     rtype object)))

(define (record-type-descriptor object)
  ((primitive-static-method <type> "make" <type> (<java.lang.Class>))
   ((primitive-virtual-method <object> "getClass"
			      <java.lang.Class> ())
    object)))

(define (record-type-name rtd)
  (symbol->string
   ((primitive-virtual-method <class-type> "getName" <String> ())
    rtd)))

(define (record-type-field-names rtd)
  ((primitive-static-method <record> "typeFieldNames"
			    <list> (<class-type>))
   rtd))

#| THESE CAN BE FUNCTIONS WHEN WE HAVE BETTER INLINING:
(define (primitive-array-new element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArrayNew> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-set element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArraySet> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-get element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArrayGet> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-length element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArrayLength> (<gnu.bytecode.Type>))
   element-type))
... etc ...
|#
(define-syntax primitive-array-new
  (syntax-rules ()
		((primitive-array-new element-type)
		 (constant-fold
		  (primitive-constructor
		   <gnu.kawa.reflect.ArrayNew> (<gnu.bytecode.Type>))
		  element-type))))
(define-syntax primitive-array-set
  (syntax-rules ()
		((primitive-array-set element-type)
		 (constant-fold
		  (primitive-constructor
		   <gnu.kawa.reflect.ArraySet> (<gnu.bytecode.Type>))
		  element-type))))
(define-syntax primitive-array-get
  (syntax-rules ()
		((primitive-array-get element-type)
		 (constant-fold
		  (primitive-constructor
		   <gnu.kawa.reflect.ArrayGet> (<gnu.bytecode.Type>))
		  element-type))))
(define-syntax primitive-array-length
  (syntax-rules ()
		((primitive-array-length element-type)
		 (constant-fold
		  (primitive-constructor
		   <gnu.kawa.reflect.ArrayLength> (<gnu.bytecode.Type>))
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
		((primitive-set-field ctype fname ftype)
		 (constant-fold
		  (primitive-constructor
		   <kawa.lang.SetFieldProc>
		   (<gnu.bytecode.ClassType> <String> <gnu.bytecode.Type>
					     <int>))
		  ctype fname ftype 1 #|PUBLIC|#))))

(define-syntax primitive-get-static
  (syntax-rules ()
		((primitive-get-static ctype fname ftype)
		 (constant-fold
		  (primitive-constructor
		   <gnu.kawa.reflect.StaticGet>
		   (<gnu.bytecode.ClassType> <String> <gnu.bytecode.Type>
					     <int>))
		  ctype fname ftype 9 #|PUBLIC|STATIC|#))))
(define-syntax primitive-set-static
  (syntax-rules ()
		((primitive-set-static ctype fname ftype)
		 (constant-fold
		  (primitive-constructor
		   <gnu.kawa.reflect.StaticSet>
		   (<gnu.bytecode.ClassType> <String> <gnu.bytecode.Type>
					     <int>))
		  ctype fname ftype 9 #|PUBLIC|STATIC|#))))

(define (subtype? (t1 <type>) (t2 <type>)) <boolean>
  ((primitive-virtual-method <type> "isSubtype" <boolean> (<type>))
   t1 t2))

#|
(define (field-location object (name <symbol>))
  ((primitive-static-method <kawa.lang.FieldLocation> "make"
                            <kawa.lang.FieldLocation> (<object> <symbol>))
   object name))

(define (method object (name <symbol>))
  ((primitive-static-method <kawa.lang.method> "make"
                            <kawa.lang.method> (<object> <symbol>))
   object name))
|#
