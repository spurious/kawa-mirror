(test-init "objects" 15)

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
