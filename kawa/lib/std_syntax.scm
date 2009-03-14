(require <kawa.lib.prim_syntax>)

;;; Definitions for some standard syntax.

(module-export cond case and or let let* letrec do delay
	       syntax-object->datum datum->syntax-object with-syntax
	       generate-temporaries define-procedure add-procedure-properties
	       identifier? free-identifier=?
	       syntax-source syntax-line syntax-column)

;;; COND

(define-syntax cond
  (syntax-rules (else =>)

		((cond (else result1 result2 ...))
		 (begin result1 result2 ...))

		((cond (else result1 result2 ...) clause1 ...)
		 (%syntax-error "else clause must be last clause of cond"))

		((cond (test => result))
		 (%let ((temp test))
		   (if temp (result temp))))

		((cond (test => result) clause1 clause2 ...)
		 (%let ((temp test))
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
				   (%let ((tmp key))
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

(define-syntax %lang-boolean
  (syntax-rules ()
    ((%lang-boolean value)
     (make <gnu.expr.QuoteExp>
       (invoke
	(gnu.expr.Language:getDefaultLanguage)
	'booleanObject value)))))

;;; AND

(define-syntax (and f)
  (syntax-case f ()
	       ((and) (%lang-boolean #t))
	       ((and test) (syntax test))
	       ((and test1 test2 ...)
		(syntax (%let ((x test1))
			  (if x (and test2 ...) x))))))
;;; OR

(define-syntax (or f)
  (syntax-case f ()
	       ((or) (%lang-boolean #f))
	       ((or test) (syntax test))
	       ((or test1 test2 ...)
		(syntax (%let ((x test1))
			  (if x x (or test2 ...)))))))

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
     ;; We destructure the bindings to simplify %let's job.
     (%let (binding ...) . body))
    ;; Alternative definition would be simpler, but makes more work for
    ;; the compiler to optimize it - and still doesn't do quite as well.
    ;;((%let-lambda1 (binding ...) () body)
    ;;(%let-init binding) ...))
    ((let name (binding ...) . body)
     ((letrec ((name (%let-lambda1 (binding ...) () body)))
	name)
      (%let-init binding) ...))))

;;; LET*

(define-syntax let*
  (syntax-rules ()
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

(define-syntax delay (syntax-rules ()
				   ((delay expression)
				    (make <kawa.lang.Promise> (lambda () expression)))))

;; Helper routines for define-procedure.
(define (add-procedure-properties
	 (proc :: <gnu.expr.GenericProc>)
	 #!rest (args :: <object[]>)) :: <void>
  (invoke proc 'setProperties args))

(define-syntax define-procedure
  (syntax-rules (:: <gnu.expr.GenericProc>)
		((define-procedure name args ...)
		 (begin
		   ;; The GenericProc has to be allocated at init time, for
		   ;; the sake of require, while the actual properties may
		   ;; need to be evaluated at module-run-time.
		   (define-constant name :: <gnu.expr.GenericProc>
		     (make <gnu.expr.GenericProc> 'name))
		   (add-procedure-properties name args ...)))))


(define (syntax-object->datum obj)
  (kawa.lang.Quote:quote obj))

(define (datum->syntax-object template-identifier obj)
  (kawa.lang.SyntaxForm:makeWithTemplate template-identifier obj))

(define (generate-temporaries list)
  (let loop ((n (kawa.lang.Translator:listLength list)) (lst '()))
    (if (= n 0) lst
	(loop (- n 1) (make <pair> (datum->syntax-object list (gnu.expr.Symbols:gentemp)) lst)))))


(define (identifier? form) :: <boolean>
  (or (gnu.mapping.Symbol? form)
      (and (kawa.lang.SyntaxForm? form)
	   (kawa.lang.SyntaxForm:isIdentifier form))))

(define (free-identifier=? id1 id2) :: <boolean>
  (kawa.lang.SyntaxForm:freeIdentifierEquals id1 id2))

(define (syntax-source form)
  (cond ((instance? form <kawa.lang.SyntaxForm>)
	 (syntax-source (*:.form (as <kawa.lang.SyntaxForm> form))))
	((instance? form <gnu.lists.PairWithPosition>)
	 (let ((str (*:getFileName (as  <gnu.lists.PairWithPosition> form))))
	   (if (eq? str #!null) #f  str)))
	(else
	 #f)))

(define (syntax-line form)
  (cond ((instance? form <kawa.lang.SyntaxForm>)
	 (syntax-line (*:.form (as <kawa.lang.SyntaxForm> form))))
	((instance? form <gnu.lists.PairWithPosition>)
	 (*:getLineNumber (as <gnu.lists.PairWithPosition> form)))
	(else
	 #f)))

;; zero-origin for compatility with MzScheme.
(define (syntax-column form)
  (cond ((instance? form <kawa.lang.SyntaxForm>)
	 (syntax-line (*:.form (as <kawa.lang.SyntaxForm> form))))
	((instance? form <gnu.lists.PairWithPosition>)
	 (- (*:getColumnNumber (as <gnu.lists.PairWithPosition> form)) 0))
	(else
	 #f)))

;;; The definition of include is based on that in the portable implementation
;;; of syntax-case psyntax.ss, whixh is again based on Chez Scheme.
;;; Copyright (c) 1992-2002 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;; Converted to use syntax-rules from the psyntax.ss implementation.

(define-syntax with-syntax
   (syntax-rules ()
     ((with-syntax () e1 e2 ...)
      (begin e1 e2 ...))
     ((with-syntax ((out in)) e1 e2 ...)
      (syntax-case in () (out (begin e1 e2 ...))))
     ((with-syntax ((out in) ...) e1 e2 ...)
      (syntax-case (list in ...) ()
		   ((out ...) (begin e1 e2 ...))))))
