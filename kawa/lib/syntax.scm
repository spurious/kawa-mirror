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

(define-syntax defmacro
  (syntax-rules ()
		((defmacro name pattern . forms)
		 (%define-macro name (lambda pattern . forms)))))

(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . pattern) . forms)
     (%define-macro name (lambda pattern . forms)))
    ((define-macro name function)
     (%define-macro name function))))

(define (gentemp) :: <symbol>
  (invoke-static <gnu.expr.Symbols> 'gentemp))

(define-syntax when (syntax-rules ()
				  ((when cond exp ...)
				   (if cond (begin exp ...)))))

(define-syntax unless (syntax-rules ()
				  ((when cond exp ...)
				   (if (not cond) (begin exp ...)))))

(define (catch key (thunk :: <procedure>) (handler :: <procedure>))
  (try-catch (thunk)
	     (ex <kawa.lang.NamedException>
		 (invoke ex 'applyHandler key handler))))

(define-syntax (try-finally x)
  (syntax-case x ()
	       ((_ try-part finally-part)
		(make <gnu.expr.TryExp>
		  (syntax->expression (syntax try-part))
		  (syntax->expression (syntax finally-part))))))

(define-syntax (synchronized x)
  (syntax-case x ()
	       ((_ object . body)
		(make <gnu.expr.SynchronizedExp>
		  (syntax->expression (syntax object))
		  (syntax-body->expression (syntax body))))))

(define (identifier? form) :: <boolean>
  (and (instance? form <kawa.lang.SyntaxForm>)
       (kawa.lang.SyntaxForm:isIdentifier form)))

(define (free-identifier=? id1 id2) :: <boolean>
  (kawa.lang.SyntaxForm:freeIdentifierEquals id1 id2))

;; SRFI-11  Copyright (C) Lars T Hansen (1999). All Rights Reserved.

;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain
;; it or assist in its implementation may be prepared, copied,
;; published and distributed, in whole or in part, without restriction
;; of any kind, provided that the above copyright notice and this
;; paragraph are included on all such copies and derivative
;; works. However, this document itself may not be modified in any
;; way, such as by removing the copyright notice or references to the
;; Scheme Request For Implementation process or editors, except as
;; needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.

;; The limited permissions granted above are perpetual and will not be
;; revoked by the authors or their successors or assigns.

;; This document and the information contained herein is provided on
;; an "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE
;; ANY RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS
;; FOR A PARTICULAR PURPOSE.

(define-syntax let-values
  (syntax-rules ()
    ((let-values (?binding ...) ?body0 ?body1 ...)
     (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
    
    ((let-values "bind" () ?tmps ?body)
     (let ?tmps ?body))
    
    ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
     (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
    
    ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
     (call-with-values 
       (lambda () ?e0)
       (lambda ?args
         (let-values "bind" ?bindings ?tmps ?body))))
    
    ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
    
    ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (call-with-values
       (lambda () ?e0)
       (lambda (?arg ... . x)
         (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () ?body0 ?body1 ...)
     (begin ?body0 ?body1 ...))

    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     (let-values (?binding0)
       (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

;; COND-EXPAND implementation from http://srfi.schemers.org/srfi-0/srfi-0.html
;; Copyright (C) Marc Feeley (1999). All Rights Reserved.
;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain
;; it or assist in its implementation may be prepared, copied,
;; published and distributed, in whole or in part, without restriction
;; of any kind, provided that the above copyright notice and this
;; paragraph are included on all such copies and derivative
;; works. However, this document itself may not be modified in any
;; way, such as by removing the copyright notice or references to the
;; Scheme Request For Implementation process or editors, except as
;; needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.
;; The limited permissions granted above are perpetual and will not be
;; revoked by the authors or their successors or assigns.
;; This document and the information contained herein is provided on
;; an "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE
;; ANY RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS
;; FOR A PARTICULAR PURPOSE.

(define-syntax cond-expand
  (syntax-rules (and or not else)
    ((cond-expand) (%syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand (feature-id . body) . more-clauses)
     (%cond-expand (feature-id . body) . more-clauses))))

(define-syntax %cond-expand
  (lambda (x)
    (syntax-case x ()
		 ((_ (test . then) . more-clauses)
		  (if (invoke-static <kawa.standard.IfFeature> 'testFeature
				     (syntax test))
		      (syntax (begin . then))
		      (syntax (cond-expand . more-clauses)))))))

;; RECEIVE implementation from http://srfi.schemers.org/srfi-8/srfi-8.html
;; Copyright (C) John David Stone (1999). All Rights Reserved.
;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain
;; it or assist in its implementation may be prepared, copied,
;; published and distributed, in whole or in part, without restriction
;; of any kind, provided that the above copyright notice and this
;; paragraph are included on all such copies and derivative
;; works. However, this document itself may not be modified in any
;; way, such as by removing the copyright notice or references to the
;; Scheme Request For Implementation process or editors, except as
;; needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.
(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))
