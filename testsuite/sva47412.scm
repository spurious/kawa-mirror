(define-simple-class A ()
	((*init* x) #!void)
)
(define-simple-class B (A)
	((*init* x)
		(invoke-special A (this) '*init* x)
		(define i 2)
		(format #t "x:~w i:~w~%" x i)
	)
)

(make B 12)

;; Output: x:12 i:2
