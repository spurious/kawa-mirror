(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.reflection>)
(require <kawa.lib.lists>)

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

(define-syntax define-syntax-case
   (syntax-rules ()
     ((define-syntax-case name literals . parts)
      (define-syntax name
        (lambda (form)
          (syntax-case form literals . parts))))))

(define-syntax when (syntax-rules ()
				  ((when cond exp ...)
				   (if cond (begin exp ...)))))

(define-syntax unless (syntax-rules ()
				  ((when cond exp ...)
				   (if (not cond) (begin exp ...)))))

(define-rewrite-syntax try-finally
  (lambda (x)
    (syntax-case x ()
      ((_ try-part finally-part)
       (make <gnu.expr.TryExp>
         (syntax->expression (syntax try-part))
         (syntax->expression (syntax finally-part)))))))

(define-rewrite-syntax synchronized
  (lambda (x)
    (syntax-case x ()
      ((_ object . body)
       (make <gnu.expr.SynchronizedExp>
         (syntax->expression (syntax object))
         (syntax-body->expression (syntax body)))))))

(define (identifier-list? obj) ::boolean
  (and (>= (kawa.lang.Translator:listLength obj) 0)
       (let loop ((obj obj))
	 (syntax-case obj ()
	   ((x . y)
	    (and (identifier? #'x) (loop #'y)))
	   (() #t)
	   (_ #f)))))

(define (identifier-pair-list? obj) ::boolean
  (and (>= (kawa.lang.Translator:listLength obj) 0)
       (let loop ((obj obj))
	 (syntax-case obj ()
	   (((from to) . y)
	    (and (identifier? #'from) (identifier? #'to) (loop #'y)))
	   (() #t)
	   (_ #f)))))

(define (import-handle-only name list)
  ;; FIXME handle if list element is a syntax object
  (if (memq name list) name #!null))
(define (import-handle-except name list)
  ;; FIXME handle if list element is a syntax object
  (if (memq name list) #!null name))
(define (import-handle-prefix name prefix)
  ;; FIXME handle if list element is a syntax object
  (if (eq? name #!null) #!null
      (let ((str (make java.lang.StringBuilder)))
        (str:append (prefix:toString))
        (str:append (name:toString))
        (gnu.mapping.SimpleSymbol:valueOf (str:toString)))))
(define (import-handle-rename name rename-pairs)
  (if (pair? rename-pairs)
      (if (eq? name (caar rename-pairs))
	  (cadar rename-pairs)
	  (import-handle-rename name (cdr rename-pairs)))
      name))
(define (import-mapper list)
  (lambda (name)
    (let loop ((l list) (n name))
      (if (or (eq? n #!null) (null? l))
	  n
	  (loop (cdr l) ((caar l) n (cdar l)))))))

(define-syntax import
  (syntax-rules ()
    ((import import-spec ...)
     (begin (%import import-spec ()) ...))))

(define-syntax-case %import (library only except prefix rename)
  ((_ (rename import-set . pairs) mapper)
   (if (identifier-pair-list? #'pairs)
       #`(%import import-set #,(cons (cons import-handle-rename #`pairs) #`mapper))
       (syntax-error (syntax rest) "invalid 'rename' clause in import")))
  ((_ (only import-set . ids) mapper)
   (if (identifier-list? #'ids)
       #`(%import import-set #,(cons (cons import-handle-only #`ids) #`mapper))
       (syntax-error (syntax ids) "invalid 'only' identifier list")))
  ((_ (except import-set . ids) mapper)
   (if (identifier-list? #'ids)
       #`(%import import-set #,(cons (cons import-handle-except #`ids) #`mapper))
       (syntax-error (syntax ids) "invalid 'except' identifier list")))
  ((_ (prefix import-set pfix) mapper)
   #`(%import import-set #,(cons (cons import-handle-prefix #`pfix) #`mapper)))
  ((_ (prefix import-set . rest) mapper)
   (syntax-error (syntax rest) "invalid prefix clause in import"))
  ((_ (library libref) mapper)
   #`(kawa.standard.ImportFromLibrary:instance libref #,(import-mapper (syntax-object->datum (syntax mapper)))))
  ((_ libref mapper)
   #`(kawa.standard.ImportFromLibrary:instance libref #,(import-mapper (syntax-object->datum (syntax mapper))))))

;; LET-VALUES implementation from SRFI-11, by Lars T Hansen.
;; http://srfi.schemers.org/srfi-11/srfi-11.html

;; This code is in the public domain.

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

(define-syntax (case-lambda form)
  (syntax-case form ()
    ((_ . cl)
     #`(gnu.expr.GenericProc:makeWithoutSorting
	. #,(let loop ((clauses #'cl))
	     (syntax-case clauses ()
	       (((formals . body) . rest)
		#`((lambda formals . body) . #,(loop #'rest)))
	       (()
		'())
	       (rest
		(list (syntax-error (syntax rest)
				    "invalid case-lambda clause")))))))))

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

(define-syntax define-alias-parameter
  (syntax-rules ()
    ((define-alias-parameter name type location)
     (begin
       (define-constant name :: <gnu.mapping.LocationProc>
	 (gnu.mapping.LocationProc:makeNamed 'name location))
       (gnu.mapping.LocationProc:pushConverter
	name
	(lambda (arg)
	  (try-catch
	   (as type arg)
	   (ex <java.lang.ClassCastException>
	       (let ((wt (gnu.mapping.WrongType:make ex name
						     (as <int> 1) arg)))
		 (set! (field wt 'expectedType) type)
		 (primitive-throw wt))))))))))

;; Helper macros for $string$:
;; Collect format string (assuming we're *not* inside $<<$ ... $>>$)
;; Returns list of format string fragments, to be concatenated.
(define (%string-format-format forms)
  (syntax-case forms ($format$ $<<$ $>>$)
    (() '())
    (($<<$ . rest)
     (%string-format-enclosed-format #'rest))
    ((($format$ fstr . args) . rest)
     (let ((xd (syntax->datum #'fstr)))
       (cons #'fstr (%string-format-format #'rest))))
    ((x . rest)
     (cons #'(constant-fold invoke (constant-fold invoke x 'toString)
                            'replace "~" "~~")
           (%string-format-format #'rest)))))

;; Collect format string, assuming we're inside $<<$ ... $>>$
;; Returns list of format string fragments, to be concatenated.
(define (%string-format-enclosed-format forms)
  (syntax-case forms ($<<$ $>>$)
    (() '())
    (($>>$ . rest)
     (%string-format-format #'rest))
    ((arg1 . rest)
     (cons "~a" (%string-format-enclosed-format #'rest)))))

;; Collect format arguments (assuming we're *not* inside $<<$ ... $>>$)
(define (%string-format-args forms)
  (syntax-case forms ($format$ $<<$ $>>$)
    (() '())
    (($<<$ . rest)
     (%string-format-enclosed-args #'rest))
    ((($format$ fstr arg ...) . rest)
     #`(arg ... . #,(%string-format-args #'rest)))
    ((x . rest)
     (%string-format-args #'rest))))
                         
;; Collect format arguments, assuming we're inside $<<$ ... $>>$
(define (%string-format-enclosed-args forms)
  (syntax-case forms ($format$ $<<$ $>>$)
    (() '())
    (($>>$ . rest)
     (%string-format-args #'rest))
    ((arg . rest)
     #`(arg . #,(%string-format-enclosed-args #'rest)))))

(define-syntax $string$
  (lambda (form)
    (syntax-case form ()
      (($string$ . forms)
       #`($format$ (constant-fold invoke
                                  (constant-fold string-append
                                                 . #,(%string-format-format
                                                      #'forms))
                                  'toString)
                   . #,(%string-format-args #'forms))))))

(define-syntax $format$
  (syntax-rules ()
    ((_ . args)
     (gnu.kawa.functions.Format:formatToString 0 . args))))

(define-syntax $sprintf$
  (syntax-rules ()
    ((_ fmt . args)
     (gnu.kawa.functions.Format:sprintfToString fmt . args))))

(cond-expand
 (kawa
  ;; (%symbol->construct 'sym) returns '$construct$:sym
  ;; In Kawa we lookup sym in the $construct$ package.
  (define-syntax %symbol->construct
    (syntax-rules ()
      ((%symbol->construct sym)
       (symbol sym $construct$)))))
 (else
  ;; In a Scheme without namespaces (i.e. colon not special)
  ;; construct a regular symbol.
  (define-syntax %symbol->construct
    (syntax-rules ()
      ((%symbol->construct sym)
       (string->symbol (string-append "$construct$:"
                                      (symbol->string sym))))))))
(define-syntax define-simple-constructor
  (lambda (form)
    (syntax-case form ($construct$)
      ((_ name constructor text-collector)
       (let ((cname (datum->syntax #'name (%symbol->construct (syntax->datum #'name)))))
         #`(define-syntax #,cname
             (syntax-rules ()
               ((#,cname . args)
                (%simple-construct-builder constructor text-collector () . args))))))
      ((_ name constructor)
       #`(define-simple-constructor name constructor $string$)))))

(define-syntax %simple-construct-builder
  (syntax-rules ($<<$ $>>$)
    ((%simple-construct-builder fun mkstr (seen ...) $<<$ . rest)
     (fun (mkstr seen ... $<<$ . rest)))
    ((%simple-construct-builder fun mkstr (seen ...) $>>$ . rest)
     (fun seen ... (mkstr . rest)))
    ((%simple-construct-builder fun mkstr (seen ...) x . rest)
     (%simple-construct-builder fun mkstr (seen ... x) . rest))
    ((%simple-construct-builder fun mkstr (seen ...))
     (fun (mkstr seen ... )))))
