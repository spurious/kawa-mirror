(module-static #t)

(require <module1>)

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

(define-variable dvar1 11)
(define-variable dvar2)
(define-variable dvar3 13)
(define dvar-test-1
  (with-compile-options warn-undefined-variable: #t
			(list dvar1 dvar2 dvar3)))

(define-namespace timestamp
  "class:MyTimestamp")

(define (my-compare a b)
  (timestamp:myCompareTo (as <MyTimestamp> a)
                         (as <MyTimestamp> b)))
