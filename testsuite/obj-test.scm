(test-init "Objects" 82)

;; Force procedure to be applied without being inlined:
(define-syntax force-eval
  (syntax-rules () ((force-eval proc arg ...)
                    (apply proc (list arg ...)))))

;; Force call to be compiled with (hopefully) inlining:
(define-syntax force-compile
  (syntax-rules () ((force-compile proc arg ...)
                    (let () (proc arg ...)))))

(define complex (make-record-type "complex" '(re im)))
(define make-complex (record-constructor complex))
(define z (make-complex 3 4))
(define make-rcomplex (record-constructor complex '(im re)))
(test z make-rcomplex 4 3)
(test 4 'accessor1 ((record-accessor complex 'im) z))
((record-modifier complex 're) z 5)
(test z make complex im: 4 re: 5)
(test 5 'accessor2 ((record-accessor complex 're) z))
(test #t record? z)
(test #f record? 5)
(test #t 'record-predicate ((record-predicate complex) z))
(test complex record-type-descriptor z)
(test "complex" record-type-name complex)
(test '(re im) record-type-field-names complex)

(test 20 'set! (begin (set! (z 'im) 15) (+ (z 're) (z 'im))))

;; Check name mangling and demangling of records.
(define funny-record (make-record-type 'lispy-name->foo!? '(a! b-c)))
(test "lispy-name->foo!?" record-type-name funny-record)
(test '(a! b-c) record-type-field-names funny-record)
(define make-funny-record1 (record-constructor funny-record))
(define make-funny-record2 (record-constructor funny-record '(b-c a!)))
(define lt1 (make-funny-record1 10 12))
(test 10 'accessor21 ((record-accessor funny-record 'a!) lt1))
((record-modifier funny-record 'b-c) lt1 (+ 2 (lt1 'b-c)))
(set! (lt1 'a!) 9)
(test (make-funny-record2 14 9) 'funny-record lt1)

(test '(10 \10 20 \20) 'object-with-field-1
       (let*
	   ((obj (object (<object>)
			 (fld 10)
			 ((toString) <String> fld)))
	    (val1 (field obj 'fld))
	    (str1 (as <String> obj)))
	 (begin
	   (set! (field obj 'fld) 20)
	   (let*
	       ((val2 (field obj 'fld))
		(str2 (as <String> obj)))
	     (list val1 str1 val2 str2)))))

(test '(100 \100 20 \20) 'object-with-field-2
       (let*
	   ((val0 100)
	    (obj (object (<object>)
			 (fld val0)
			 ((toString) <String> fld)))
	    (val1 (field obj 'fld))
	    (str1 (as <String> obj)))
	 (begin
	   (set! (field obj 'fld) 20)
	   (let*
	       ((val2 (field obj 'fld))
		(str2 (as <String> obj)))
	     (list val1 str1 val2 str2)))))

(test 1 'object-locals
      (let ((x (object (<java.lang.Object>) (z (lambda (x) (display x)))))) 1))

(test 1 'object-locals
      (let* ((d display)
	     (x (object (<java.lang.Object>) (z (lambda (x) (d x)))))) 1))

(test 2 'object-with-closure-1
      (length (let*
		  ((name 'x)
		   (obj (object (<java.util.zip.Adler32>))))
		(letrec ((opt
			  (lambda (args)
			    (list obj
				  (object (<java.lang.Object>
					   <java.awt.event.ItemListener>)
					  ((itemStateChanged
					    (arg <java.awt.event.ItemEvent>))
					   <void>
					   (display name) 
					   (newline)))))))
		  (opt 3)))))

(define (object-with-closure-2 c-name)
  (let* ((c-path (symbol->string c-name)) 
	 (c-obj (object (<java.lang.Object>)))) 
    (letrec ((opt (lambda (args) 
		    (if (pair? args) 
			(begin 
			  (let ((listener
				 (object (<java.lang.Object>
					  <java.awt.event.ItemListener>) 
					 ((itemStateChanged (arg <java.awt.event.ItemEvent>)) 
					  <void> 
					  (display "listener of checkbutton ") 
					  (display c-name) 
					  (display arg) 
					  (newline))))) 
			    (list c-obj listener)) 
			  (opt (cddr args)))))))
      (opt (list )))
    c-path))

(test ".x.c" object-with-closure-2 '.x.c)

(define (document-filter arg1)
  (lambda (arg2)
    (object ()
            ((toString)
             <String>
             (format #f "{arg1: ~s arg2: ~s}" arg1 arg2)))))

(test "{arg1: 23 arg2: 12}" 'object-with-closure-3
      (symbol->string (as <String> ((document-filter 23) 12))))

(define i100 (force-eval make <integer> ival: 100))
(define i200 (force-compile make <integer> ival: 200))
(test 100 'test-make-1 i100)
(test 200 'test-make-2 i200)
(define cons1 (force-eval make <pair> 7 9))
(test '(7 . 9) 'test-make-3 cons1)
(test '(9 . 6) 'test-make-3 (force-compile make <pair> 9 6))
(force-eval slot-set! cons1 'cdr 99)
(test '(7 . 99) 'test-slot-set-1 cons1)
(force-compile slot-set! cons1 'cdr (field '(88 99) 'car))
(test '(7 . 88) 'test-slot-set-2 cons1)
(set! (slot-ref cons1 'car) 8)
(test '(8 . 88) 'test-slot-set-3 cons1)
(set! (field cons1 'cdr) 55)
(test '(8 . 55) 'test-slot-set-3 cons1)
(test #t 'test-slot-ref-1
      (force-eval static-field <java.lang.Boolean> 'TRUE))
(test '() 'test-slot-ref-2
      (force-compile static-field <list> 'Empty))
(test #t 'test-slot-ref-3
      (force-eval field #f 'TRUE))
(test #f 'test-slot-ref-4
      (force-compile field #t 'FALSE))

;; Make sure objects compiled in separate compilations don't cause
;; errors (naming clashes)
(test '(1 2) (lambda ()
               ;; define each object in a separate compile
               (define obj1 (eval '(object () ((one) 1))))
               (define obj2 (eval '(object () ((two) 2))))
               (list (invoke obj1 'one)
                     (invoke obj2 'two))))

(define internal-node-name list)
(require <module2>)
(test 4 list-length-1 '(a b c d))
(test 2 list-length-3 '(a b))
(test 0 length (classify))

(test 3 length-diff1 'abcdef 'abc)
(test 3 length-diff2 'abcdef 'abc)
(test 3 length-diff3 'abcdef 'abc)

(test '(1 2 3 4) 'deldup-test list1234)

;; Test bug reported by Jocelyn Paine.
(test '(boolean #t) make-literal #t)

(test '(3 . 4) make-pair 3 4)

(require <module3>)
(test 0 list-length-4 '())

(test 24 'factorial-4 factorial-4)

(require <classes1>)
(require <classes2>)

(define obj1 (make <SimpleA>))
(test 4 slot-ref obj1 'a)
(test 6 slot-ref obj1 'b)
(test 35 'obj1-f (invoke obj1 'f 5))
(test #(y) 'lambda-method1 ((invoke obj1 'lambda-method1) 'y))
(test #(z z z) 'lambda-method2 ((invoke obj1 'lambda-method2 -1) 'z))
(slot-set! obj1 'a (+ 10 (static-field <SimpleA> 'b)))
(test "yes" slot-ref obj1 'hyphenated-field?)
(test 16 field obj1 'a)

(define obj2 (make <SimpleB>))
(test 4 field obj2 'a)
(test 6 field obj2 'b)
(test 6 static-field <SimpleB> 'b)
(test 10 slot-ref obj2 'c)
(test 1045 'obj2-f (invoke obj2 'f 15))

(define obj3 (make <SimpleC> d: 25))
(test 4 'obj3-a (slot-ref obj3 'a))
(test 6 'obj3-b (slot-ref obj3 'b))
(test 10 'obj3-c (slot-ref obj3 'c))
(test 25 slot-ref obj3 'd)
(test 24 slot-ref obj3 'e)

(define obj4 (make <ClsC>))
(test 14 'obj4-b (slot-ref obj4 'b))
(test 22 'obj4-c (slot-ref obj4 'c))
(test 44 'obj4-f (invoke obj4 'f 2))

(define obj5 (make <ClsD>))
(test 23 'obj5-d (slot-ref obj5 'd))

(define obj6 (make <ClsE>))
(set! (field obj6 'e) (- (field obj6 'e) 10))
(test 29 'obj6-e (slot-ref obj6 'e))
(test 156 'obj6-f (invoke obj6 'f 7))

(require <MyFunc>)
(test '(1 2 3) my-func-1 2 3)
(require <MyModule>)
(test '(#t 5 6) my-func-t 5 6)

(define-record-type pare
  (kons x y-part)
  pare?
  (x kar set-kar!)
  (y-part kdr))
(test #t pare? (kons 1 2))
(test #f pare? (cons 1 2))
(test 1 kar (kons 1 2))
(test 2 kdr (kons 1 2))
(test 3 'set-kdr!
      (let ((k (kons 1 2)))
	(set-kar! k 3)
	(kar k)))

(define obj-with-let
  (object ()
	  ((meth v)
	   (let ((n v) (r (this)))
	     (list n)))) )
(test '(3) 'ff (invoke obj-with-let 'meth 3))

(define (create-main-frame)
  (object ()
   (myField init-form:
        (format #f "this: ~a\n" (this))
        )))
(define main-frame (create-main-frame))
(test (format #f "this: ~a\n" main-frame) field main-frame 'myField)

(define simple-date (make <SimpleDateTest>))
(define-namespace date "class:java.util.Date")
(test (+ 1900 (date:get-year (date:new)))
      invoke simple-date 'get-year)

;; Test for Savannah bug #4289
(define pa-data (pa-new 10))
(pa-setter pa-data 5 99)
(test 99 pa-getter pa-data 5)
(test #!null pa-getter pa-data 6)
(test 10 pa-length pa-data)
