;; This module should be compiled with --full-tailcalls.
;(module-static #t)

;; Bug reported 2005-05-08 by dominique.boucher@nuecho.com.
(define (neg-abs x)
  (if (< x 0)
      x
      (- x)))

;; A number of tests for names containing colons.
(define prefix-test '(1))
(define prefix-test:var2 '(2))
(set-car! prefix-test (+ prefix-test:car 10))
;; The syntax prefix-test:var2:car works but is not supported.
(set-car! prefix-test:var2 (+ prefix-test:var2:car 10))
(define prefix-test:var2:var3 '(3))
(set-car! prefix-test:var2:var3 (+ prefix-test:var2:var3:car 10))
(define prefix-test:filler:var4 '(4))
(set-car! prefix-test:filler:var4 (+ prefix-test:filler:var4:car 10))
(define prefix-test-list
  (list (cons 'prefix-test prefix-test)
	(cons 'prefix-test:var2 prefix-test:var2)
	(cons 'prefix-test:var2:var3 prefix-test:var2:var3)
	(cons 'prefix-test:filler:var4 prefix-test:filler:var4)))


