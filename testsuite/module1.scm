(module-static #f)

(define (my-factorial n)
  (if (<= n 1)
      1
      (* n (my-factorial (- n 1)))))

(define-private (list-length-1 (x :: <list>)) :: <double>
   (length x))
(define (list-length-2 x) :: <int>
  (include-relative "included-1.scm")
  (inexact->exact z2))

(require 'list-lib)

(define-syntax deldup
  (syntax-rules ()
   ((deldup list)
    (delete-duplicates list))))

(define (call-to-first x)
  (first x))

(define-namespace date "class:java.util.Date")

(define-syntax namespace-syntax-test
  (syntax-rules ()
    ((namespace-syntax-test)
     (date:parse "6 Sep 2003 UTC"))))

;; Test for Savannah bug #11508 submitted by Thomas Kirk <tk@research.att.com>:
(define-syntax make-array
  (syntax-rules ()
    ((make-array len type)
     ((primitive-array-new type) len))
    ((make-array len)
     ((primitive-array-new <java.lang.Object>) len))))
(define (make-array-fun n)
  (make-array n))

;; From Savannah bug#11822, contributed by Dean Ferreyra.
;; (Other parts of this testcase are in module3.scm and obj-test.scm.)
(define-simple-class <simpleAux> ()
  (x 5)
  ((init) (set! x 100)))
(define-syntax mA
  (syntax-rules ()
    ((_ type forms ...)
     (define-simple-class type ()
       forms ...))))
