(define (current-readtable) :: <readtable>
  (invoke-static <readtable> 'getCurrent))

(define (readtable? obj) :: <boolean>
  (instance? obj <readtable>))

#|
(define (set-syntax-from-char
	 (from-char :: <char>) (to-char :: <char>)
	 #!optional
	 (from-readtable :: <readtable> (current-readtable))
	 (to-readtable :: <readtable>)) ;; FIXME default
  ...)
|#

(define (set-macro-character (char :: <char>)
			     function
			     #!optional
			     (non-terminating :: <boolean> #f)
			     (readtable :: <readtable> (current-readtable)))
  (invoke readtable 'set char
	  (make <gnu.kawa.lispexpr.ReaderMacro> function non-terminating)))

(define (get-macro-character
	 (char :: <char>)
	 #!optional
	 (readtable :: <readtable> (current-readtable)))
  (let ((entry :: <gnu.kawa.lispexpr.ReaderMacro>
	       (invoke readtable 'lookup char)))
    (value (invoke entry 'getProcedure)
	   (invoke entry 'isNonTerminating))))

(define (make-dispatch-macro-character
	 (char :: <char>)
	 #!optional
	 (non-terminating :: <boolean> #f)
	 (readtable :: <readtable> (current-readtable)))
  (invoke readtable 'set char
	  (make <gnu.kawa.lispexpr.ReaderDispatch> non-terminating)))

(define (set-dispatch-macro-character
	 (disp-char :: <char>) (sub-char :: <char>)
	 function
	 #!optional (readtable :: <readtable> (current-readtable)))
  (let ((entry :: <gnu.kawa.lispexpr.ReaderDispatch>
	       (invoke readtable 'lookup disp-char)))
    (invoke entry 'set sub-char
	    (make <gnu.kawa.lispexpr.ReaderDispatchMacro> function))))

(define (get-dispatch-macro-table
	 (disp-char :: <char>) (sub-char :: <char>)
	 #!optional (readtable :: <readtable> (current-readtable)))
  (let* ((disp-entry :: <gnu.kawa.lispexpr.ReaderDispatch>
		     (invoke readtable 'lookup disp-char))
	 (sub-entry (invoke disp-entry 'lookup sub-char)))
    (eq? sub-entry #!null #f sub-entry)))

    
