(test-init "Miscellaneous")

;;; DSSSL spec example 11
(test '(3 4 5 6) (lambda x x) 3 4 5 6)
(test '(5 6) (lambda (x y #!rest z) z) 3 4 5 6)
(test '(3 4 5 i: 6 j: 1)
      (lambda ( x y #!optional z #!rest r #!key i (j 1))
	(list x y z i: i j: j))
      3 4 5 i: 6 i: 7)

(test #t keyword? foo:)
(test #t keyword? 'foo:)
(test #f keyword? 'foo\:)

;;; DSSSL spec example 44
(test "Argentina" keyword->string \Argentina:)

;;; DSSSL spec example 45
(test foobar: string->keyword "foobar")

(define-unit ft 12in)
(test 18in + 6in 1ft)

;;; This caused a spurious warning in earlier releases.
(test '(1 2 3) 'let (let ((x (lambda l l))) (x 1 2 3)))
