(test-init "Objects" 41)

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
((record-modifier funny-record 'b-c) lt1 14)
(test (make-funny-record2 14 10) 'funny-record lt1)

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
      (symbol->string ((document-filter 23) 12)))

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
(require <module1>)
(test 4 list-length-1 '(a b c d))
(test 2 list-length-3 '(a b))
(test 0 length (classify))

(test 3 length-diff1 'abcdef 'abc)
(test 3 length-diff2 'abcdef 'abc)
(test 3 length-diff3 'abcdef 'abc)
