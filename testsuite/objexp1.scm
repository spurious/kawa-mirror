(define (object-with-closure c-name)
  (let ((opt (lambda ()
	       (object (<java.lang.Object>)
                       ((toString) ::java.lang.String
                        (format #f "opt[~a]" c-name))
		       ((fun) ::void
			(display c-name))))))
    opt))
(define obj (object-with-closure '.x.c))
(format #t "object: ~w -> ~w~%" obj (obj))
;; Output: object: #<procedure opt> -> opt[.x.c]
