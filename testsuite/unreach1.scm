(define (bar1)
  (if (primitive-throw java.lang.NullPointerException) (list 23) 11))
;; Diagnostic: unreach1.scm:2:56: warning - unreachable code

(define (bar2)
  (list (primitive-throw java.lang.NullPointerException) 12 13))
;; Diagnostic: unreach1.scm:6:58: warning - unreachable code

(define (bar3)
  (list 12 (primitive-throw java.lang.NullPointerException) (sqrt 13)))
;; Diagnostic: unreach1.scm:10:61: warning - unreachable code

(define (bar4)
  (primitive-throw java.lang.NullPointerException)
  13)
;; Diagnostic: unreach1.scm:15:3: warning - unreachable code

(define (bar5)
  (begin (primitive-throw java.lang.NullPointerException)
         13))
;; Diagnostic: unreach1.scm:20:10: warning - unreachable code

;;; Savannah bug #35524: Unreachable code is not an error
(define (foo)
  (call-with-current-continuation
   (lambda (return)
     (let l ()
       (return #f)
       (l)))))
;; Diagnostic: unreach1.scm:29:8: warning - unreachable code
