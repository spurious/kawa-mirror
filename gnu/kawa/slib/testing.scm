;; Copyright (c) 2005, Per Bothner
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(cond-expand
 (chicken
  (require-extension syntax-case))
 (guile
  (use-modules (ice-9 syncase) (srfi srfi-9)
	       ;;(srfi srfi-34) (srfi srfi-35) - not in Guile 1.6.7
	       (srfi srfi-39)))
 (sisc
  (require-extension (srfi 9 34 35 39)))
 (else ()
  ))

(cond-expand
 (chicken
  (define-syntax test-source-location-cons%
    (syntax-rules ()
      ((test-source-location-cons% form-to-use cdr)
       (or (and-let* ((line (get-line-number 'form-to-use)))
             (cons (cons 'source-line line) cdr))
           cdr)))))
 (gauche
  (define-syntax test-source-location-cons%
    (syntax-rules ()
      ((test-source-location-cons% form-to-use cdr)
       (let ((f 'form-to-use))
         (or (and-let* (((pair? f))
                        (info (pair-attribute-get f 'source-info #f))
                        ((pair? info))
                        ((pair? (cdr info))))
               (cons (cons 'source-file (car info))
                     (cons (cons 'source-line (cadr info)) cdr)))
             cdr))))))
 (kawa
  (module-compile-options warn-undefined-variable: #t
			  warn-invoke-unknown-method: #t)
  (define-syntax source-file
    (lambda (x)
      (syntax-case x ()
		   ((_ form)
		    (let ((form (syntax-object->datum (syntax (form)))))
		      (if (instance? form <gnu.lists.PairWithPosition>)
			  (list (quote quote)
				(datum->syntax-object form (gnu.lists.PairWithPosition:getFile form)))
			  #f))))))
  (define-syntax source-line
    (lambda (x)
      (syntax-case x ()
		   ((_ form)
		    (let ((form (syntax-object->datum (syntax (form)))))
		      (if (instance? form <gnu.lists.PairWithPosition>)
			  (list (quote quote)
				(datum->syntax-object form (gnu.lists.PairWithPosition:getLine form)))
			  #f))))))
  (define-syntax test-source-location-cons%
    (syntax-rules ()
      ((test-source-location-cons% form-to-use cdr)
       (cons (cons 'source-file (source-file  form-to-use))
	     (cons (cons 'source-line (source-line  form-to-use))
		   cdr))))))
 (else
  (define-syntax test-source-location-cons%
    (syntax-rules ()
      ((test-source-location-cons% form-to-use cdr)
       cdr)))))

(cond-expand
 (srfi-9
  (define-syntax %test-record-define%
    (syntax-rules ()
      ((%test-record-define% alloc runner? (name index setter getter) ...)
       (define-record-type test-runner
	 (alloc)
	 runner?
	 (name setter getter) ...)))))
 (else
  (define test-runner-cookie% (list "test-runner"))
  (define-syntax %test-record-define%
    (syntax-rules ()
      ((%test-record-define% alloc runner? (name index getter setter) ...)
       (begin
	 (define (runner? obj)
	   (and (vector? obj)
		(> (vector-length obj) 1)
		(eq (vector-ref obj 0) test-runner-cookie%)))
	 (define (alloc)
	   (let ((runner (make-vector 23)))
	     (vector-set! runner 0 test-runner-cookie%)
	     runner))
	 (begin
	   (define (getter runner)
	     (vector-ref runner index)) ...)
	 (begin
	   (define (setter runner value)
	     (vector-set! runner index value)) ...)))))))

(%test-record-define%
 test-runner-alloc test-runner?
 ;; Cumulate count of all tests that have passed and were expected to.
 (pass-count 1 test-runner-pass-count test-runner-pass-count!)
 (fail-count 2 test-runner-fail-count test-runner-fail-count!)
 (xpass-count 3 test-runner-xpass-count test-runner-xpass-count!)
 (xfail-count 4 test-runner-xfail-count test-runner-xfail-count!)
 (skip-count 5 test-runner-skip-count test-runner-skip-count!)
 (skip-list 6 test-runner-skip-list test-runner-skip-list!)
 (fail-list 7 test-runner-fail-list test-runner-fail-list!)
 (run-list 8 test-runner-run-list test-runner-run-list!)
 (skip-save 9 test-runner-skip-save test-runner-skip-save!)
 (fail-save 10 test-runner-fail-save test-runner-fail-save!)
 (group-path 11 test-runner-group-path test-runner-group-path!)
 (on-test-begin 12 test-runner-on-test-begin test-runner-on-test-begin!)
 (on-test-end 13 test-runner-on-test-end test-runner-on-test-end!)
 ;; Call-back when entering a group. Takes (runner suite-name count).
 (on-group-begin 14 test-runner-on-group-begin test-runner-on-group-begin!)
 ;; Call-back when leaving a group.
 (on-group-end 15 test-runner-on-group-end test-runner-on-group-end!)
 ;; Call-back when leaving the outermost group.
 (on-final 16 test-runner-on-final test-runner-on-final!)
 ;; Call-back when expected number of tests was wrong.
 (on-bad-count 17 test-runner-on-bad-count test-runner-on-bad-count!)
 ;; Field can be used by test-runner for any purpose.
;; test-runner-simple uses it for a log file.
 (aux-value 18 test-runner-aux-value test-runner-aux-value!)
 (test-name 19 test-runner-test-name test-runner-test-name!)
 ;; Cumulate count of all tests that have been done.
 (total-count 20 test-runner-total-count test-runner-total-count!)
 ;; Stack (list) of (count-at-start . expected-count):
 (count-list 21 test-runner-count-list test-runner-count-list!)
 (result-alist 22 test-runner-result-alist test-runner-result-alist!)
)

(define (test-runner-reset runner)
    (test-runner-pass-count! runner 0)
    (test-runner-fail-count! runner 0)
    (test-runner-xpass-count! runner 0)
    (test-runner-xfail-count! runner 0)
    (test-runner-skip-count! runner 0)
    (test-runner-total-count! runner 0)
    (test-runner-count-list! runner '())
    (test-runner-run-list! runner #t)
    (test-runner-skip-list! runner '())
    (test-runner-fail-list! runner '())
    (test-runner-skip-save! runner '())
    (test-runner-fail-save! runner '())
    (test-runner-group-path! runner '())
    (test-runner-test-name! runner ""))

(define (test-null-callback runner) #f)

(define (test-runner-null)
  (let ((runner (test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner (lambda (runner name count) #f))
    (test-runner-on-group-end! runner test-null-callback)
    (test-runner-on-final! runner test-null-callback)
    (test-runner-on-test-begin! runner test-null-callback)
    (test-runner-on-test-end! runner test-null-callback)
    (test-runner-on-bad-count! runner (lambda (runner count expected) #f))
    runner))

(define test-log-to-file #t)

(define (test-runner-simple)
  (let ((runner (test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-test-begin! runner test-on-test-begin-simple)
    (test-runner-on-test-end! runner test-on-test-end-simple)
    (test-runner-on-bad-count! runner test-on-bad-count-simple)
    runner))

(cond-expand
 (srfi-39
  (define test-runner-current (make-parameter #f))
  (define test-runner-factory (make-parameter test-runner-simple)))
 (else
  (define test-runner-current% #f)
  (define-syntax test-runner-current
    (syntax-rules ()
      ((test-runner-current)
       test-runner-current%)
      ((test-runner-current runner)
       (set! test-runner-current% runner))))
  (define test-runner-factory% test-runner-simple)
  (define-syntax test-runner-factory
    (syntax-rules ()
      ((test-runner-factory)
       test-runner-factory%)
      ((test-runner-factory runner)
       (set! test-runner-factory% runner))))))

(define (test-%specificier-matches spec runner)
  (spec runner))

(define (test-runner-create)
  ((test-runner-factory)))

(define (test-%any-specifier-matches list runner)
  (let loop ((l list))
    (cond ((null? l) #f)
	  ((test-%specificier-matches (car l) runner) #t)
	  (else (loop (cdr l))))))

;; Returns #f, #t, or 'expected-fail.
(define (test-%should-execute-test name runner)
  (test-runner-test-name! runner name)
  (let ((run (test-runner-run-list runner)))
    (and
     (or (eqv? run #t)
	 (test-%any-specifier-matches run runner))
     (cond ((test-%any-specifier-matches
	     (test-runner-skip-list runner)
	     runner)
	    #f)
	   ((test-%any-specifier-matches
	     (test-runner-fail-list runner)
	     runner)
	    'expected-fail)
	   (else #t)))))

(define (test-%begin suite-name count)
  (if (not (test-runner-current))
      (test-runner-current (test-runner-create)))
  (let ((runner (test-runner-current)))
    ((test-runner-on-group-begin runner) runner suite-name count)
    (test-runner-skip-save! runner
			       (cons (test-runner-skip-list runner)
				     (test-runner-skip-save runner)))
    (test-runner-fail-save! runner
			       (cons (test-runner-fail-list runner)
				     (test-runner-fail-save runner)))
    (test-runner-count-list! runner
			     (cons (cons (test-runner-total-count runner)
					 count)
				   (test-runner-count-list runner)))
    (test-runner-group-path! runner (cons suite-name
					(test-runner-group-path runner)))))

(define-syntax test-begin
  (syntax-rules ()
    ((test-begin suite-name)
     (test-%begin suite-name #f))
    ((test-begin suite-name count)
     (test-%begin suite-name count))))

(define (test-on-group-begin-simple runner suite-name count)
  (if (null? (test-runner-group-path runner))
      (begin
	(display "%%%% Starting test ")
	(display suite-name)
	(if test-log-to-file
	    (let* ((log-file-name
		    (if (string? test-log-to-file) test-log-to-file
			(string-append suite-name ".log")))
		   (log-file (open-output-file log-file-name)))
	      (display "%%%% Starting test " log-file)
	      (display suite-name log-file)
	      (newline log-file)
	      (test-runner-aux-value! runner log-file)
	      (display "  (Writing full log to \"")
	      (display log-file-name)
	      (display "\")")))
	(newline)))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(begin
	  (display "Group begin: " log)
	  (display suite-name log)
	  (newline log))))
  #f)

(define (test-on-group-end-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(begin
	  (display "Group end" log)
	  ;; (display suite-name log) FIXME
	  (newline log))))
  #f)

(define (test-on-bad-count-write runner count expected-count port)
  (display "*** Total number of tests was " port)
  (display count port)
  (display " but should be " port)
  (display expected-count port)
  (display ". ***" port)
  (newline port)
  (display "*** Discrepancy indicates testsuite error or exceptions. ***" port)
  (newline port))

(define (test-on-bad-count-simple runner count expected-count)
  (test-on-bad-count-write runner count expected-count (current-output-port))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(test-on-bad-count-write runner count expected-count log))))

(define (test-final-report1 value label port)
  (if (> value 0)
      (begin
	(display label port)
	(display value port)
	(newline port))))

(define (test-final-report-simple runner port)
  (test-final-report1 (test-runner-pass-count runner)
		      "# of expected passes      " port)
  (test-final-report1 (test-runner-xfail-count runner)
		      "# of expected failures    " port)
  (test-final-report1 (test-runner-xpass-count runner)
		      "# of unexpected successes " port)
  (test-final-report1 (test-runner-fail-count runner)
		      "# of unexpected failures  " port)
  (test-final-report1 (test-runner-skip-count runner)
		      "# of skipped tests        " port))

(define (test-on-final-simple runner)
  (test-final-report-simple runner (current-output-port))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(test-final-report-simple runner log))))

;; FIXME doesn't check that suite-name matches
;; nor does try to recover from a mismatch (by extra pops).
(define (test-end% suite-name)
  (let* ((r (test-runner-current))
	 (count-list (test-runner-count-list r))
	 (expected-count (cdar count-list))
	 (saved-count (caar count-list))
	 (group-count (- (test-runner-total-count r) saved-count)))
    (test-runner-group-path! r (cdr (test-runner-group-path r)))
    (test-runner-skip-list! r (car (test-runner-skip-save r)))
    (test-runner-skip-save! r (cdr (test-runner-skip-save r)))
    (test-runner-fail-list! r (car (test-runner-fail-save r)))
    (test-runner-fail-save! r (cdr (test-runner-fail-save r)))
    (if (and expected-count
	     (not (= expected-count group-count)))
	((test-runner-on-bad-count r) r group-count expected-count))
    (test-runner-count-list! r (cdr count-list))
    ((test-runner-on-group-end r) r)
    (if (null? (test-runner-group-path r))
	((test-runner-on-final r) r))))

(define-syntax test-end
  (syntax-rules ()
    ((test-end)
     (test-end% #f))
    ((test-end suite-name)
     (test-end% suite-name))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (if (test-%should-execute-test suite-name (test-runner-current))
	 (dynamic-wind
	     (lambda () (test-begin suite-name))
	     (lambda () . body)
	     (lambda () (test-end  suite-name)))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
		    (dynamic-wind
			(lambda () #f)
			(lambda () form)
			(lambda () cleanup-form))))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name #f cleanup-form))
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))

(define (test-on-test-begin-simple runner)
 (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(let* ((results (test-runner-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (source-form (assq 'source-form results))
	       (test-name (assq 'test-name results)))
	  (display "Test begin:" log)
	  (newline log)
	  (if test-name (test-write-result1 test-name log))
	  (if source-file (test-write-result1 source-file log))
	  (if source-line (test-write-result1 source-line log))
	  (if source-file (test-write-result1 source-form log))))))

(define (test-on-test-end-simple runner)
  (let ((log (test-runner-aux-value runner))
	(kind (test-result-ref runner 'result-kind)))
    (if (memq kind '(fail xpass))
	(let* ((results (test-runner-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (test-name (assq 'test-name results)))
	  (if (or source-file source-line)
	      (begin
		(if source-file (display (cdr source-file)))
		(display ":")
		(if source-line (display (cdr source-line)))
		(display ": ")))
	  (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
	  (if test-name
	      (begin
		(display " ")
		(display (cdr test-name))))
	  (newline)))
    (if (output-port? log)
	(begin
	  (display "Test end:" log)
	  (newline log)
	  (let loop ((list (test-runner-result-alist runner)))
	    (if (pair? list)
		(let ((pair (car list)))
		  ;; Write out properties not written out by on-test-begin.
		  (if (not (memq (car pair)
				 '(test-name source-file source-line source-form)))
		      (test-write-result1 pair log))
		  (loop (cdr list)))))))))

(define (test-write-result1 pair port)
  (display "  " port)
  (display (car pair) port)
  (display ": " port)
  (write (cdr pair) port)
  (newline port))

(define-syntax test-result-ref
  (syntax-rules ()
    ((test-result-ref runner pname)
     (test-result-ref runner pname #f))
    ((test-result-ref runner pname default)
     (let ((p (assq pname (test-runner-result-alist runner))))
       (if p (cdr p) default)))))

(define (test-result-set! runner pname value)
  (let* ((alist (test-runner-result-alist runner))
	 (p (assq pname alist)))
    (if p
	(set-cdr! p value)
	(test-runner-result-alist! runner (cons (cons pname value) alist)))))

(define (test-result-clear runner)
  (test-runner-result-alist! runner '()))

(define (test-result-remove runner pname)
  (let* ((alist (test-runner-result-alist runner))
	 (p (assq pname alist)))
    (if p
	(test-runner-result-alist! runner
				   (let loop ((r alist))
				     (if (eq? r p) (cdr r)
					 (cons (car r) (loop (cdr r)))))))))

(define (test-result-kind . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (test-result-ref runner 'result-kind)))

(define (test-passed? . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (memq (test-result-ref runner 'result-kind) '(pass xpass))))

(define (test-%report-result)
  (let* ((r (test-runner-current))
	 (result-kind (test-result-kind r)))
    (case result-kind
      ((pass)
       (test-runner-pass-count! r (+ 1 (test-runner-pass-count r))))
      ((fail)
       (test-runner-fail-count!	r (+ 1 (test-runner-fail-count r))))
      ((xpass)
       (test-runner-xpass-count! r (+ 1 (test-runner-xpass-count r))))
      ((xfail)
       (test-runner-xfail-count! r (+ 1 (test-runner-xfail-count r))))
      (else
       (test-runner-skip-count! r (+ 1 (test-runner-skip-count r)))))
    (test-runner-total-count! r (+ 1 (test-runner-total-count r)))
    ((test-runner-on-test-end r) r)))

(cond-expand
 (guile
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch% test-expression)
       (catch #t (lambda () test-expression) (lambda (key . args) #f))))))
 (kawa
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch% test-expression)
       (try-catch test-expression
		  (ex <java.lang.Throwable>
		      ex))))))
 (srfi-34
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch% test-expression)
       (guard (err (else #f)) test-expression)))))
 (chicken
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch% test-expression)
       (condition-case test-expression (ex () #f))))))
 (else
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch% test-expression)
       test-expression)))))
	    
(cond-expand
 (kawa
  (define (test-source-line2 form)
    (cons (cons 'source-form (syntax-object->datum form))
	  (if (instance? form <gnu.lists.PairWithPosition>)
	      (list
	       (cons 'source-file
		     (make <string> (gnu.lists.PairWithPosition:getFile form)))
	       (cons 'source-line
		     (gnu.lists.PairWithPosition:getLine form)))))))
 (else
  (define (test-source-line2 form)
    '())))

(define-syntax test-comp2body%
  (syntax-rules ()
		((test-comp2body% r comp expected expr)
		 ;; FIXME: ((test-runner-on-test-begin r) r)
		 ;; or should call-back be done after the should-execute test?
		 ;; Perhaps:
		 ;; Do should-execute-test, which may set 'skip.
		 ;; Do: ((test-runner-on-test-begin r) r)
		 ;; Latter may also set 'skip
		 ;; (if (not 'skip) ...)
		 (let ((should (test-%should-execute-test "" r))) ;FIXME
		   ((test-runner-on-test-begin r) r)
		   (let ((result
			  (if should ;; FIXME reload if-skip
			      (let ((exp expected))
				(test-result-set! r 'expected-value exp)
				(let ((res (test-evaluate-with-catch% expr)))
				  (test-result-set! r 'actual-value res)
				  (let ((result (comp exp res)))
				    (if (eq? should 'expected-fail)
					(if result 'xpass 'xfail)
					(if result 'pass 'fail)))))
			      'skip)))
		     (test-result-set! r 'result-kind result)
		     (test-%report-result))))))

(define (test-appromixate= error)
  (lambda (value expected)
    (and (>= value (- expected error))
	 (<= value (+ expected error)))))

(define-syntax test-comp1body%
  (syntax-rules ()
		((test-comp2body% r expr)
		 ;; FIXME: ((test-runner-on-test-begin r) r)
		 ;; or should call-back be done after the should-execute test?
		 ;; Perhaps:
		 ;; Do should-execute-test, which may set 'skip.
		 ;; Do: ((test-runner-on-test-begin r) r)
		 ;; Latter may also set 'skip
		 ;; (if (not 'skip) ...)
		 (let ((should (test-%should-execute-test "" r))) ; FIXME
		   ((test-runner-on-test-begin r) r)
		   (let ((result
			  (if should ;; FIXME reload if-skip
			      (let ((result (test-evaluate-with-catch% expr)))
				(test-result-set! r 'actual-value result)
				(if (eq? should 'expected-fail)
				    (if result 'xpass 'xfail)
				    (if result 'pass 'fail)))
			      'skip)))
		     (test-result-set! r 'result-kind result)
		     (test-%report-result))))))

(cond-expand
 (kawa
  ;; Should be made to work for any Scheme with syntax-case
  ;; However, I haven't gotten the quoting working.  FIXME.
  (define (test-assert% x)
    (syntax-case (list x (list 'quote (test-source-line2 x))) ()
      (((mac tname expr) line)
       (syntax
	(let* ((r (test-runner-current))
	       (name tname))
	  (test-runner-result-alist! r (cons (cons 'test-name tname) line))
	  (test-comp1body% r expr))))
      (((mac expr) line)
       (syntax
	(let* ((r (test-runner-current)))
	  (test-runner-result-alist! r line)
	  (test-comp1body% r expr))))))
  (define-syntax test-assert
    (lambda (x) (test-assert% x)))
  (define (test-comp2% comp x)
    (syntax-case (list x (list 'quote (test-source-line2 x)) comp) ()
      (((mac tname expected expr) line comp)
       (syntax
	(let* ((r (test-runner-current))
	       (name tname))
	  (test-runner-result-alist! r (cons (cons 'test-name tname) line))
	  (test-comp2body% r comp expected expr))))
      (((mac expected expr) line comp)
       (syntax
	(let* ((r (test-runner-current)))
	  (test-runner-result-alist! r line)
	  (test-comp2body% r comp expected expr))))))
  (define-syntax test-eqv
    (lambda (x) (test-comp2% (syntax eqv?) x)))
  (define-syntax test-eq
    (lambda (x) (test-comp2% (syntax eq?) x)))
  (define-syntax test-equal
    (lambda (x) (test-comp2% (syntax equal?) x)))
  (define-syntax test-approximate ;; FIXME - needed for non-Kawa
    (lambda (x)
      (syntax-case (list x (list 'quote (test-source-line2 x))) ()
      (((mac tname expected expr error) line)
       (syntax
	(let* ((r (test-runner-current))
	       (name tname))
	  (test-runner-result-alist! r (cons (cons 'test-name tname) line))
	  (test-comp2body% r (test-appromixate= error) expected expr))))
      (((mac expected expr error) line)
       (syntax
	(let* ((r (test-runner-current)))
	  (test-runner-result-alist! r line)
	  (test-comp2body% r (test-appromixate= error) expected expr))))))))
 (else
  (define-syntax test-assert
    (syntax-rules ()
      ((test-assert tname test-expression)
       (let* ((r (test-runner-current))
	      (name tname))
	 (test-runner-result-alist!
	  r
	  (test-source-location-cons% tname '((test-name . tname))))
	 (test-comp1body% r expr)))
      ((test-assert test-expression)
       (let* ((r (test-runner-current)))
	 (test-runner-result-alist! r '())
	 (test-comp1body% r expr)))))
  (define-syntax test-comp2%
    (syntax-rules ()
      ((test-comp2% comp tname expected expr)
       (let* ((r (test-runner-current))
	      (name tname))
	 (test-runner-result-alist! r (list (cons 'test-name tname)))
	 (test-comp2body% r comp expected expr)))
      ((test-comp2% comp expected expr)
       (let* ((r (test-runner-current)))
	 (test-runner-result-alist! r '())
	 (test-comp2body% r comp expected expr)))))
  (define-syntax test-equal
    (syntax-rules ()
      ((test-equal . rest)
       (test-comp2% equal? . rest))))
  (define-syntax test-eqv
    (syntax-rules ()
      ((test-eqv . rest)
       (test-comp2% eqv? . rest))))
  (define-syntax test-eq
    (syntax-rules ()
      ((test-eq . rest)
       (test-comp2% eq? . rest))))
  (define-syntax test-approximate
    (syntax-rules ()
      ((test-approximate tname expected expr error)
       (test-comp2% (test-appromixate= error) tname expected expr))
      ((test-approximate expected expr error)
       (test-comp2% (test-appromixate= error) expected expr))))))

(cond-expand
 (guile
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       (catch #t (lambda () expr #f) (lambda (key . args) #t))))))
 (srfi-35
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% #t expr)
       (guard (ex (else #t))
	      (begin expr #f)))
      ((test-error% etype expr)
       (guard (ex ((condition-has-type? ex etype) #t) (else #f))
	      (begin expr #f))))))
 (srfi-34
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       (guard (ex (else #t))
	      (begin expr #f))))))
 (chicken
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       (condition-case (begin expr #f)
                       (ex () #t))))))
 (kawa
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       (try-catch (begin expr #f)
		  (ex <java.lang.Throwable> #t))))))
 (else
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       (begin expr #f))))))

(define-syntax test-error
  (syntax-rules ()
    ((test-error name etype expr)
     (test-assert name (test-error% etype expr)))
    ((test-error etype expr)
     (test-assert (test-error% etype expr)))
    ((test-error expr)
     (test-assert (test-error% #t expr)))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
           (lambda () (test-runner-current runner))
           (lambda () form ...)
           (lambda () (test-runner-current saved-runner)))))))

;;; Predicates

;; Coerce a form to a prediacte function:
(define-syntax test-make-predicate%
  (syntax-rules ()
    ((test-make-predicate% form) form))) ;; FIXME

(define (test-match-nth% n count)
  (let ((i 0))
    (lambda (runner)
      (set! i (+ i 1))
      (and (>= i n) (< i (+ n count))))))

(define-syntax test-match-nth
  (syntax-rules ()
    ((test-match-nth n)
     (test-match-nth n 1))
    ((test-match-nth n count)
     (test-match-nth% n count))))

(define (test-match-all% pred-list)
  (lambda (runner)
    (let loop ((l pred-list))
      (if (null? l) #t
	  (and ((car l) runner)
	       (loop (cdr l)))))))
  
(define-syntax test-match-all
  (syntax-rules ()
    ((test-match-all pred)
     (test-make-predicate% pred))
    ((test-match-all pred ...)
     (test-match-all% (list (test-make-predicate% pred) ...)))))

(define (test-match-any% pred-list)
  (lambda (runner)
    (let loop ((l pred-list))
      (if (null? l) #f
	  (or ((car l) runner)
	      (loop (cdr l)))))))
  
(define-syntax test-match-any
  (syntax-rules ()
    ((test-match-any pred ...)
     (test-match-any% (list (test-make-predicate% pred) ...)))))

(define-syntax test-skip
  (syntax-rules ()
    ((test-skip . pred)
     (let ((runner (test-runner-current)))
       (test-runner-skip-list! runner
				  (cons (test-match-all . pred)
					(test-runner-skip-list runner)))))))

(define-syntax test-expect-fail
  (syntax-rules ()
    ((test-expect-fail . pred)
     (let ((runner (test-runner-current)))
       (test-runner-fail-list! runner
				  (cons (test-match-all . pred)
					(test-runner-fail-list runner)))))))

(define (test-match-named name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner))))
