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
