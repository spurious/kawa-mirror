(test-begin "libs" 33)

(import (srfi :2 and-let*))

(test-equal 1 (and-let* () 1))
(test-equal 2 (and-let* () 1 2))
(test-equal #t (and-let* ()))

(test-equal #f (let ((x #f)) (and-let* (x))))
(test-equal 1 (let ((x 1)) (and-let* (x))))
(test-equal #f (and-let* ((x #f)) ))
(test-equal 1  (and-let* ((x 1)) ))
(test-error (eval '(and-let* ( #f (x 1)))))
(test-equal #f (and-let* ( (#f) (x 1)) ))
(test-error (eval '(and-let* (2 (x 1)))))
(test-equal 1 (and-let* ( (2) (x 1)) ))
(test-equal 2 (and-let* ( (x 1) (2)) ))
(test-equal #f (let ((x #f)) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x)  )))
(test-equal 2 (let ((x 1)) (and-let* (x) (+ x 1))))
(define xf #f)
(test-equal #f (and-let* (xf) (+ xf 1)))
(test-equal 2 (let ((x 1)) (and-let* (((positive? x))) (+ x 1))))
(test-equal #t (let ((x 1)) (and-let* (((positive? x))) )))
(test-equal #f (let ((x 0)) (and-let* (((positive? x))) (+ x 1))))
(test-equal 3 (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1))) )
;(must-be-a-syntax-error
;  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
;)

(test-equal 2 (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal 2 (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))
(test-equal #f (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal #f (and-let* (xf ((positive? xf))) (+ xf 1)))
(test-equal #f (and-let* (((begin xf)) ((positive? xf))) (+ xf 1)))

(test-equal #f  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f (and-let* (xf (y (- xf 1)) ((positive? y))) (/ xf y)))
(test-equal 3/2  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))

(define (symbol-parts s::symbol)
  (list (symbol-local-name s) (symbol-namespace-uri s) (symbol-prefix s)))

(test-equal '("abc:def" "" "")
	    (symbol-parts '|abc:def|))
;(test-equal '("abc:def" "" "")
;	    (symbol-parts 'abc:def))

(require 'xml)

(test-equal '("abc" "URI" "")
	    (symbol-parts (element-name #<abc xmlns="URI"/>)))

;; Contributed by Helmut Eller.
(define version-1
 '((module-export foo)
   (module-static #t)
   (module-compile-options
    warn-invoke-unknown-method: #t
    warn-undefined-variable: #t)
   (define (foo) (bar))
   (define (bar) "version 1")))
(define version-2
 '((module-export foo)
   (module-static #t)
   (module-compile-options
    warn-invoke-unknown-method: #t
    warn-undefined-variable: #t)
   (define (foo) (bar))
   (define (bar) "version 2")))
(define (test-ev-req)
  (let* ((file (java.io.File:createTempFile "foo" ".scm"))
	 (filename (file:getAbsolutePath))
	 (now (lambda () (java.lang.System:currentTimeMillis)))
	 (cache-time (max gnu.expr.ModuleManager:LAST_MODIFIED_CACHE_TIME
			  1000))
	 (wait (lambda () (let* ((date (file:lastModified)))
			    (let loop ()
			      (when (< (- (now) date) (* 2 cache-time))
				(sleep 0.5))))))
	 (write-forms (lambda (forms)
			(wait)
			(call-with-output-file filename
			  (lambda (stream)
			    (format stream "~{~s~%~}" forms)))
			(wait))))
    (try-finally
     (begin
       (write-forms version-1)
       (eval `(begin (require ,filename)
		     (define foo-1 foo)
		   (define result-1 (foo-1)))
	     (interaction-environment))
       (write-forms version-2)
       (eval `(begin (require ,filename)
		     (define result-2 (foo-1))
		     (list result-1 result-2))
	     (interaction-environment)))
     (delete-file filename))))
(test-equal
 '("version 1" "version 2")
 (test-ev-req))

(test-end)
