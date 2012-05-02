(define (foo)
  (call/cc
   (lambda (return)
     (let ((a 1))
       (let loop ((a a))
         (let ((finish (lambda (a) (return #f))))
           (finish a)
           (let ((a 2))
             (loop a))))))))
;; Diagnostic: sva35728.scm:8:12: warning - unreachable code
(format #t "foo: ~s~%" (foo))
;; Output: foo: #f
