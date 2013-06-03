(eval '(define (foo) 'a))
(eval '(define x foo))
(eval '(let ((x (lambda () 'b)))
         (format #t "~w; ~w; ~w~%" (x) x (foo))))
;; Output: b; #<procedure x>; a

;; This simplified/hacked version of next-leaf-generator from test.scm
;; used to cause a VerifyError (due to a bug in setCallersNeedStaticLink
;; in LambdaExp.java).
(define (next-leaf-generator eot)
  (letrec ((cont (lambda (x)
                   (cond (x (list x eot))
                         (else
                          (set! cont
                                (lambda (x) eot))
                          (cont #t))))))
    (cont #f)))
(format #t "lg: ~a~%" (next-leaf-generator 'eot))
;; Output: lg: (#t eot)
