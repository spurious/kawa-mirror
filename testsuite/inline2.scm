(import (scheme base)
        (scheme write))

(define (bluh)
  (define (foo c) #f)
  (define (bar f) #f)
  (define (baz n) 2)

  (define (snoo)
    (bar (lambda (i) #f))
    (foo #f))

  (define (blee)
    (let loop ((ret 0))
      (foo (lambda () (loop 0)))))

  (define (pup)
    (let ((l (make-vector 288)))
      (bar (lambda (i) (vector-set! l i 5)))
      (blee)))

  (define (blerg)
    (let* ((res (snoo)))
      (blee)))

  (let* ((t (baz 2)))
    (let loop ((res
                (case t
                  ((0) 5)
                  ((1) (pup))
                  ((2) (blerg))
                  (else
                   #f))))
      #f)))

(format #t "bluh -> ~w~%" (bluh))
;; Output: bluh -> #f
