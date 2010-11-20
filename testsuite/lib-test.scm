(test-begin "libs" 71)

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

(require 'syntax-utils)
(test-equal 'x (expand 'x))
(test-equal 1 (expand 1))
(test-equal '(let ((x 10)) x) (expand '(let ((x 10)) x)))
(test-equal '(lambda (x) x) (expand '(lambda (x) x)))
(test-equal '(if x 'a 'b) (expand '(if x 'a 'b)))
(test-equal '(set x 10) (expand '(set! x 10)))
(test-equal '(begin (x) (y)) (expand '(begin (x) (y))))
(test-equal "foo" (expand "foo"))
(test-equal '(quote (a b c)) (expand ''(a b c)))
(test-equal #f (expand '#f))
(test-equal #t (expand '#t))
(test-equal '(if (= x 1) (quote a) (if (= x 2) (quote b)))
      (expand '(cond ((= x 1) 'a)
			  ((= x 2) 'b))))
(test-equal '((let ((loop #!undefined)) 
	  (begin (set loop (lambda () (loop))) loop)))
      (expand '(let loop () (loop))))
(test-equal '(let ((x #!undefined)) (set x 10))
      (expand '(define x 10)))
(test-equal '(as <java.lang.String> (quote a))
      (expand '(as String 'a)))

(import (srfi :41 streams))

(define strm123
  (stream-cons 1
    (stream-cons 2
      (stream-cons 3
        stream-null))))
(test-equal 1 (stream-car strm123))
(test-equal 2 (stream-car (stream-cdr strm123)))
(test-equal #f
            (stream-pair?
             (stream-cdr
              (stream-cons (/ 1 0) stream-null))))
(test-equal #f (stream? (list 1 2 3)))
(test-equal 3 (stream-length strm123))

(define iter
  (stream-lambda (f x)
    (stream-cons x (iter f (f x)))))
(define nats (iter (lambda (x) (+ x 1)) 0))
(test-equal 1 (stream-car (stream-cdr nats)))

(define stream-add
  (stream-lambda (s1 s2)
    (stream-cons
      (+ (stream-car s1) (stream-car s2))
      (stream-add (stream-cdr s1)
                  (stream-cdr s2)))))
(define evens (stream-add nats nats))
(test-equal 0 (stream-car evens))
(test-equal 2 (stream-car (stream-cdr evens)))
(test-equal 4 (stream-car (stream-cdr (stream-cdr evens))))

(define (square x) (* x x))
(test-equal '(81 9) (stream->list (stream-map square (stream 9 3))))
(define (sigma f m n)
  (stream-fold + 0
    (stream-map f (stream-range m (+ n 1)))))
(test-equal 338350 (sigma square 1 100))

(test-equal '(1 2 3 2 1)
            (stream->list
             (stream-concat
              (stream
               (stream 1 2) (stream) (stream 3 2 1)))))
(test-equal
 '(0 1 4 9 16 25 36 49 64 81)
 (stream->list 10
               (stream-map (lambda (x) (* x x))
                           (stream-from 0))))
(test-equal '(3 4 3 4 3 4 3)
            (stream->list 7
             (stream-constant 3 4)))
(test-equal  '(1 3 5 7 9)
             (stream->list 5
                           (stream-filter odd? (stream-from 0))))

(test-equal '(0 4 16 36 64)
            (stream->list
             (stream-of (* x x)
                        (x in (stream-range 0 10))
                        (even? x))))
(test-equal '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2))
            (stream->list
             (stream-of (list a b)
                        (a in (stream-range 1 4))
                        (b in (stream-range 1 3)))))
(test-equal '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
            (stream->list
             (stream-of (list i j)
                        (i in (stream-range 1 5))
                        (j in (stream-range (+ i 1) 5)))))

(define (stream-partition pred? strm)
  (stream-unfolds
    (lambda (s)
      (if (stream-null? s)
          (values s '() '())
          (let ((a (stream-car s))
                (d (stream-cdr s)))
            (if (pred? a)
                (values d (list a) #f)
                (values d #f (list a))))))
    strm))

(test-equal '((1 3 5) (2 4))
            (call-with-values
                (lambda ()
                  (stream-partition odd?
                                    (stream-range 1 6)))
              (lambda (odds evens)
                (list (stream->list odds)
                      (stream->list evens)))))

(define primes (let ()
                 (define-stream (next base mult strm)
                   (let ((first (stream-car strm))
                         (rest (stream-cdr strm)))
                     (cond ((< first mult)
                            (stream-cons first
                                         (next base mult rest)))
                           ((< mult first)
                            (next base (+ base mult) strm))
                           (else (next base
                                       (+ base mult) rest)))))
                 (define-stream (sift base strm)
                   (next base (+ base base) strm))
                 (define-stream (sieve strm)
                   (let ((first (stream-car strm))
                         (rest (stream-cdr strm)))
                     (stream-cons first
                                  (sieve (sift first rest)))))
                 (sieve (stream-from 2))))

(test-equal 997
            (stream-car
             (stream-reverse
              (stream-take-while
               (lambda (x) (< x 1000))
               primes))))

(define-stream (stream-finds eql? obj strm)
  (stream-of (car x)
    (x in (stream-zip (stream-from 0) strm))
    (eql? obj (cadr x))))
(define (stream-find eql? obj strm)
  (stream-car
    (stream-append
      (stream-finds eql? obj strm)
      (stream #f))))
(test-equal 2
            (stream-find char=? #\l
                         (list->stream
                          (string->list "hello"))))
(test-equal #f
            (stream-find char=? #\l
                         (list->stream
                          (string->list "goodbye"))))

(define power-table
  (stream-of
    (stream-of (expt m n)
      (m in (stream-from 1)))
      (n in (stream-from 2))))
(test-equal '(1 8 27 64 125 216 343 512 729 1000)
            (stream->list 10 (stream-ref power-table 1)))

(test-end)
