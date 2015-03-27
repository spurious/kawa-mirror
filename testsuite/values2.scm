(define (consume1 x)
  (format #t "value:~a.~%" x))

(define (test1)
  (call-with-values
      (lambda () 123)
    consume1))
(define (test2)
  (call-with-values
      (lambda () 127)
    (lambda (x) (format #t "value:~a.~%" x))))

(test1)
;; Output: value:123.
(test2)
;; Output: value:127.
