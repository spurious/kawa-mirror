(define verbose #f)

(define pass-count 0)
(define fail-count 0)
(define xfail-count 0)
(define xpass-count 0)

(define *log-file* #f)

(define test-name "<unknown>")

;;; Set this (to an explanatory string) if the next test is known to fail.
(define fail-expected #f)

;;; The current section.
(define cur-section #f)
;;; The section when we last emitted a message.
(define last-section #f)

(define (TEST-INIT name)
  (set! test-name name)
  (set! *log-file* (open-output-file (string-append name ".log")))
  (display (string-append "%%%% Starting test " name) *log-file*)
  (newline *log-file*)
  (display (string-append "%%%% Starting test " name
			  "  (Writing full log to \"" name ".log\")"))
  (newline)
  (set! pass-count 0)
  (set! xpass-count 0)
  (set! fail-count 0)
  (set! xfail-count 0))

(define (display-section port)
  (display "SECTION" port)
  (do ((l cur-section (cdr l)))
      ((null? l) #f)
    (write-char #\Space port)
    (display (car l) port))
  (newline port))

(define (maybe-report-section)
  (and cur-section *log-file* (not (eq? cur-section last-section))
       (begin (display-section (current-output-port))
	      (set! last-section cur-section))))

(define (SECTION . args)
  (set! cur-section args)
  (display-section (or *log-file* (current-output-port)))
  (set! last-section #f)
  #t)
(define record-error (lambda (e) (set! errs (cons (list cur-section e) errs))))

(define (report-pass port fun args res)
  (display (if fail-expected "XPASS:" "PASS: ") port)
  (write (cons fun args) port)
  (display "  ==> " port)
  (write res port)
  (newline port))

(define (report-fail port fun args res expect)
  (display (if fail-expected (string-append "XFAIL (" fail-expected "): ")
	       "FAIL: ") port)
  (write (cons fun args) port)
  (display "  ==> " port)
  (write res port)
  (display " BUT EXPECTED " port)
  (write expect port)
  (newline port))

(define (test expect fun . args)
  ((lambda (res)
     (cond ((equal? expect res)
	    (if fail-expected
		(set! xpass-count (+ xpass-count 1))
		(set! pass-count (+ pass-count 1)))
	    (if *log-file*
		(report-pass *log-file* fun args res))
	    (cond ((or verbose fail-expected)
		   (maybe-report-section)
		   (report-pass (current-output-port) fun args res))))
	   (#t
	    (if fail-expected
		(set! xfail-count (+ xfail-count 1))
		(set! fail-count (+ fail-count 1)))
	    (if *log-file*
		(report-fail *log-file* fun args res expect))
	    (cond ((or verbose (not fail-expected))
		   (maybe-report-section)
		   (report-fail (current-output-port) fun args res expect)))))
     (set! fail-expected #f))
   (if (procedure? fun) (apply fun args) (car args))))

(define (report1 value string)
  (cond ((> value 0)
	 (display string)	 (display value)	 (newline)
	 (cond (*log-file*
		(display string *log-file*)
		(display value *log-file*)
		(newline *log-file*))))))

(define (test-report)
  (report1 pass-count  "# of expected passes      ")
  (report1 xfail-count "# of expected failures    ")
  (report1 xpass-count  "# of unexpected successes ")
  (report1 fail-count  "# of unexpected failures  ")
  (cond (*log-file*
	 (close-output-port *log-file*)
	 (set! *log-file* #f))))
