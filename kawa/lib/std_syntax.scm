;;; Definitions for some standard syntax.

;;; COND

(define-syntax cond
  (syntax-rules (else =>)

		((cond (else result1 result2 ...))
		 (begin result1 result2 ...))

		((cond (else result1 result2 ...) clause1 ...)
		 (%syntax-error "else clause must be last clause of cond"))

		((cond (test => result))
		 (let ((temp test))
		   (if temp (result temp))))

		((cond (test => result) clause1 clause2 ...)
		 (let ((temp test))
		   (if temp
		       (result temp)
		       (cond clause1 clause2 ...))))

		((cond (test))
		 test)

		((cond (test) clause1 clause2 ...)
		 (or test (cond clause1 clause2 ...)))

		((cond (test result1 result2 ...))
		 (if test (begin result1 result2 ...)))

		((cond (test result1 result2 ...) clause1 clause2 ...)
		 (if test
		     (begin result1 result2 ...)
		     (cond clause1 clause2 ...)))))

;;; CASE

(define-syntax case (syntax-rules ()
				  ((case key clauses ...)
				   (let ((tmp key))
				     (%case tmp clauses ...)))))

(define-syntax %case (syntax-rules (else)
				   ((%case key (else expression ...))
				    (begin expression ...))
				   ((%case key (else expression ...) . junk)
				    (%syntax-error
				     "junk following else (in case)"))
				   ((%case key
					   ((datum ...) expression ...))
				    (if (%case-match key datum ...)
					(begin expression ...)))
				   ((%case key
					   ((datum ...) expression ...)
					   clause more ...)
				    (if (%case-match key datum ...)
					(begin expression ...)
					(%case key clause more ...)))))
					  
(define-syntax %case-match (syntax-rules ()
					 ((%case-match key datum)
					  (eqv? key (quote datum)))
					 ((%case-match key datum more ...)
					  (or (eqv? key 'datum)
					      (%case-match key more ...)))))

;;; AND

(define-syntax and
  (syntax-rules ()
		((and) #t)
		((and test) test)
		((and test1 test2 test3 ...)
		 (and (if test1 test2 #f) test3 ...))))

;;; LET (including named let)

(define-syntax %let-lambda1
  (syntax-rules (::)
		((%let-lambda1 ((var type init) . in) out body)
		 (%let-lambda1 in ((var type) . out) body))
		((%let-lambda1 ((var :: type init) . in) out body)
		 (%let-lambda1 in ((var type) . out) body))
		((%let-lambda1 ((var init) . in) out body)
		 (%let-lambda1 in (var . out) body))
		((%let-lambda1 () out body)
		 (%let-lambda2 out () body))))

;;; This is just to reverse the argument list yielded by %let-lambda1.
(define-syntax %let-lambda2
  (syntax-rules ()
		((%let-lambda2 (arg . in) out body)
		 (%let-lambda2 in (arg . out) body))
		((%let-lambda2 () out body)
		 (lambda out . body))))

(define-syntax %let-init
  (syntax-rules (::)
		((%let-init (var init))
		 init)
		((%let-init (var :: type init))
		 init)
		((%let-init (var type init))
		 init)
		((%let-init (var))
		 (%syntax-error "let binding with no value"))
		((%let-init (var a b c))
		 (%syntax-error
		  "let binding must have syntax: (var [type] init)"))))

(define-syntax let
  (syntax-rules ()
		((let (binding ...) . body)
		 (%let (binding ...) . body))
		; Alternative definition would be simpler, but makes more
		; work for compiler to optimize it - and still doesn't
		; do quite as well.
		;((%let-lambda1 (binding ...) () body)
		;(%let-init binding) ...))
		((let name (binding ...) . body)
		 ((letrec ((name (%let-lambda1 (binding ...) () body)))
		    name)
		  (%let-init binding) ...))))

;;; LET*

(define-syntax let* (syntax-rules ()
				 ((let* () . body) (%let () . body))
				 ((let* (var-init) . body)
				  (%let (var-init) . body))
				 ((let* (var-init . bindings) . body)
				  (%let (var-init)
				    (let* bindings . body)))
				 ((let* bindings . body)
				  (%syntax-error
				   "invalid bindings list in let*"))
				 ((let* . body)
				  (%syntax-error
				   "missing bindings list in let*"))))

;;; LETREC

(define-syntax letrec
  (syntax-rules ()
    ((letrec bindings . body)
     (%letrec1 () bindings . body))))

(define-syntax %letrec1
  (syntax-rules (::)
    ((%letrec1 done ((x :: type init) . bindings) . body)
     (%letrec1 ((x :: type #!undefined) . done) bindings (set! x init) . body))
    ((%letrec1 done ((x init) . bindings) . body)
     (%letrec1 ((x #!undefined) . done) bindings (set! x init) . body))
    ((%letrec1 done () . body)
     (%let done . body))))

;;; DO

;;; Helper macro for do, to handle optional step.
(define-syntax %do-step
  (syntax-rules ()
		((%do-step variable step) step)
		((%do-step variable) variable)))

(define-syntax do
  (syntax-rules (::)
		;;; Handle type specifier - but only for one variable.
		;;; I don't know how to handle the general case, by just
		;;; using syntax-rules, so fix that later.  FIXME.
		((do ((name :: type init step))
		     (test . result) commands ...)
		 (letrec ((%do%loop
			   (lambda ((name type))
			     (if test
				 (begin #!void . result)
				 (begin commands ...
					(%do%loop step))))))
		   (%do%loop init)))
		((do ((name init . step) ...)
		     (test . result) commands ...)
		 ;; The identifier %do%loop is optimized specially ...
		 (letrec ((%do%loop
			   (lambda (name ...)
			     (if test
				 (begin #!void . result)
				 (begin commands ...
					(%do%loop (%do-step name . step) ...))))))
		   (%do%loop init ...)))))

;;; DELAY

(define (%make-promise x)
  ((primitive-constructor <kawa.lang.Promise> (<gnu.mapping.Procedure>)) x))

(define-syntax delay (syntax-rules ()
				   ((delay expression)
				    (%make-promise (lambda () expression)))))
