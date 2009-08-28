(require <kawa.lib.prim_syntax>)

(define-syntax (provide form)
  (syntax-case form ()
    ((provide 'feature)
     (cons (syntax define-constant)
	   (cons (datum->syntax-object
		  form
		  (string->symbol
		   (string-append "%provide%"
				  (symbol->string
				   (syntax-object->datum (syntax feature))))))
		 (syntax (:: <int> 123)))))
    ((_ . rest)
     (syntax-error form "provide requires a quoted feature-name"))))

(define-syntax test-begin
  (syntax-rules ()
    ((test-begin suite-name)
     (begin
       (cond-expand (srfi-64 #!void) (else (require 'srfi-64)))
       (%test-begin suite-name #f)))
    ((test-begin suite-name count)
     (begin
       (cond-expand (srfi-64 #!void) (else (require 'srfi-64)))
       (%test-begin suite-name count)))))

(define-syntax module-uri
  (lambda (form)
    (syntax-case form ()
      ((_)
       (gnu.kawa.functions.GetModuleClass:getModuleClassURI
	(gnu.expr.Compilation:getCurrent))))))

(define-syntax resource-url
  (syntax-rules ()
    ((resource-url uri)
     (gnu.text.URLPath:valueOf
      (((((module-uri):resolve uri):toURL):openConnection):getURL)))))

;;; The definition of include is based on that in the portable implementation
;;; of syntax-case psyntax.ss, which is again based on Chez Scheme.
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
(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ((p (open-input-file fn)))
          (let f ()
            (let ((x (read p)))
              (if (eof-object? x)
                  (begin (close-input-port p) '())
                  (<pair> (datum->syntax-object k x) (f))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax-object->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

(define-syntax (include-relative x)
  (syntax-case x ()
	       ((_ filename)
		(let* ((path-pair :: <gnu.lists.PairWithPosition>
				  (syntax-object->datum (syntax (filename))))
		       (base :: path (path-pair:getFileName))
		       (fname ((path-pair:getCar):toString)))
		  (list
		   (datum->syntax-object (syntax filename) 'include)
		   (datum->syntax-object
		    (syntax filename)
		    ((base:resolve fname):toString)))))))

#|
(define-syntax source-file
  (lambda (x)
    (syntax-case x ()
		 ((_ form)
		  (let ((form (syntax-object->datum (syntax (form)))))
		    (if (instance? form <gnu.lists.PairWithPosition>)
			(list (quote quote)
			      (datum->syntax-object form (gnu.lists.PairWithPosition:getFileName form)))
			#f))))))
|#
