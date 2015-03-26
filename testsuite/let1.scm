(let ((bar (let ((abck (lambda () "Ok."))) abck)))
  (let ((baz (list (bar))))
    (let ((foo (bar))) (format #t "~a~%" foo))))

;; Output: Ok.
