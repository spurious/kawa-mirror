;; Submitted by Andrea Girotto <andrea.girotto@gmail.com> 15 Nov 2009
;; as part of Savannah 15 Nov 2009 bug report #28022:
;; "case-lambda (srfi 16) not working with strings".
;; Converted to SRFI-64-style, and a couple of bugs fixed, by Per Bothner.

(test-begin "srfi-16-test" 12)

(define add-proc
  (case-lambda
    (() 0)
    ((x) x)
    ((x y) (+ x y))
    ((x y z) (+ (+ x y) z))
    (args (apply + args)) ) )

(test-equal  0 (add-proc))
(test-equal  1 (add-proc 1))
(test-equal  5 (add-proc 2 3))
(test-equal 15 (add-proc 4 5 6))
(test-equal 34 (add-proc 7 8 9 10))

(define list-proc
  (case-lambda
    (()        '())
    ((x l)     (cons x l))
    ((x l1 l2) (cons x (list-proc l1 l2)))
    (x         x) ) )

(test-equal '()       (list-proc))
(test-equal '(a)      (list-proc 'a))
(test-equal '(a . b)  (list-proc 'a 'b))
(test-equal '(a b . c)  (list-proc 'a 'b 'c))

(define string-proc
  (case-lambda
    (() "null-string")
    ((x) (string-append "to-string:" x))
    (l (apply string-append (cons "append:" l))) ) )

(test-equal "null-string"          (string-proc))
(test-equal "to-string:this"       (string-proc "this"))
(test-equal "append:thisthatthose" (string-proc "this" "that" "those"))

(test-end)
