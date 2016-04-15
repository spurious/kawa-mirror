(define-simple-class A ()
	((*init*)
		(for-each
			(lambda
				(n)
				(print n)
			)
			'(1 2 3)
		)
	)
	((print n)
		(format #t "~D" n)
	)
)

(A)
(newline)
;; Output: 123
