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

;; Based on bug report 2016-02-11 from OKUMURA Yuki <mjt@cltn.org>.
(define (test3 b)
  (if (eq? b 'NONE)
      (values 'NONE #f)
      (values 'OTHERS #f)))
(call-with-values (lambda () (test3 'X))
  (lambda vals (format #t "split-value:~a.~%" vals)))
;; Output: split-value:(OTHERS #f).
