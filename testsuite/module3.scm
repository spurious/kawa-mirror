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
