(eval '(define (foo) 'a))
(eval '(define x foo))
(eval '(let ((x (lambda () 'b)))
         (format #t "~w; ~w; ~w~%" (x) x (foo))))
;; Output: b; #<procedure x>; a
