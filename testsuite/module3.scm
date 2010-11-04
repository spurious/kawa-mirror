(module-static #t)

(require <module1>)

(module-export dvar-test-1 factorial-4 check-fluid-let *VAR*
	       namespace-syntax-call list-length-4 my-compare
	       test3-import1 get3-mod0-v2 set3-mod0-v2
	       pa-new pa-getter pa-setter pa-length iarr-set mB test1-import0)

(define (get3-mod0-v1) :: <object> mod0-v1)
(define (set3-mod0-v1 x) (set! mod0-v1 x))
(define (get3-mod0-v2) :: <object> mod0-v2)
(define (set3-mod0-v2 x) (set! mod0-v2 x))

(define (test3-import1)
  (let ((gv1 (get3-mod0-v1)))
    (set3-mod0-v1 (- gv1 9))
    (list gv1 mod0-v1 mod0-v2 (mod0-f1) (mod0-m1))))

;; This didn't use to compile, because of macro expansion problems.
(define (test3-import2)
  (mod0-m1))

(define factorial-4 (my-factorial 4))

(define (list-length-4 arg)
  (list-length-2 arg))

;; Test for Savannah bug #4289
(define (pa-getter data index)
  (let ((getter (primitive-array-get <java.lang.Object>)))
    (getter data index)))
(define (pa-setter data index val)
  (let ((setter (primitive-array-set <java.lang.Object>)))
    (setter data index val)))
(define (pa-length data)
  (let ((lengther (primitive-array-length <java.lang.Object>)))
    (lengther data)))
(define (pa-new size)
  (let ((newer (primitive-array-new <java.lang.Object>)))
    (newer size)))

(define (namespace-syntax-call)
  (namespace-syntax-test))

;; Test for Savannah bug #5651
(define (iarr-set (array :: <int[]>) (index :: <int>) (value :: <int>))
  (let ((setter (primitive-array-set <int>)))
    (setter array index value)))

(define-variable dvar1
  (with-compile-options warn-undefined-variable: #f
			(+ (get-mod0-v1) 1))) ;; 11
(define-variable dvar2)
(define-variable dvar3 13)
(define dvar-test-1
  (with-compile-options warn-undefined-variable: #t
			(list dvar1 dvar2 dvar3)))

(define-namespace timestamp "class:MyTimestamp")
;; This also works: (define-alias timestamp <MyTimestamp>)
;; but not (intentionally): (define-alias timestamp  "class:MyTimestamp")

(define (my-compare a b)
  (timestamp:myCompareTo (as <MyTimestamp> a)
                         (as <MyTimestamp> b)))

;; Test for Savannah bug #11578
(define *VAR* 'A)
(define (get-var) *VAR*)
(define (check-fluid-let sym)
  (fluid-let ((*VAR* sym))
    (get-var)))

;; Based on Savannah bug#11822, contributed by Dean Ferreyra.
;; (Other parts of this testcase are in module1.scm and obj-test.scm.)
(define-namespace simpleAux <simpleAux>)
(define-syntax mB
  (syntax-rules ()
    ((_ type name)
     (mA type 
       ((fn o)
        (simpleAux:init o)
        (list (slot-ref o 'x) name))))))
