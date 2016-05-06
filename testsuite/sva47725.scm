(module-compile-options warn-as-error: #t)

(define-alias ClassType gnu.bytecode.ClassType)
(define-alias Filter gnu.bytecode.Filter)

(define-simple-class MFilter (Filter)
	((select o ::java.lang.Object) ::boolean
		#t
	)
)

(define c ::ClassType (ClassType:make "java.util.HashMap"))
(define nm::int (*:getMethods c (MFilter) 2 #!null))
(format #t "~a~%~!" (if (> nm 100) 'ok 'bad))
;; Output: ok
