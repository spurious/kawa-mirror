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

;;; test old reader bugs 
(test '(b) cdr '(a .(b))) 
(test "foo" cdr '(a ."foo")) 
(test 'a car '(a #||#)) 

(define (try-vector-ref vec index)
  (try-catch (vector-ref vec index)
	     (ex <java.lang.IndexOutOfBoundsException>
		 "Bad array index")))

(test 3 try-vector-ref #(1 2 3) 2)
(test "Bad array index" try-vector-ref #(1 2 3) 10)

(define (test-catch)
  (let* ((x 0)
	 (y (catch 'key
		   (lambda ()
		     (set! x 2)
		     (throw 'key 10)
		     (set! x 1000))
		   (lambda (key arg)
		     (set! x (* x arg))
		     (+ x 10)))))
    (list x y)))

(test '(20 30) test-catch)

(section "new-line handling")
;;; Test that #\return and #\newline are read robustly.

(define cr-test-string "a \"bRLc\" dRklLXY")
(do ((i 0 (+ i 1)))
    ((= i (string-length cr-test-string)) #t)
  (if (char=? #\R (string-ref cr-test-string i))
      (string-set! cr-test-string i #\Return))
  (if (char=? #\L (string-ref cr-test-string i))
      (string-set! cr-test-string i #\Linefeed)))
(call-with-input-string
 cr-test-string
 (lambda (iport)
   (test 1 input-port-column-number iport)
   (test 1 input-port-line-number iport)
   (test 'a read iport)
   (test "b\nc" read iport)
   (test 'd read iport)
   (test 'kl read iport)
   (test 'XY read iport)
   (test #!eof read iport)))

(call-with-input-string
 cr-test-string
 (lambda (iport)
   (test #\a read-char iport)
   (test #\Space read-char iport)
   (test #\" read-char iport)
   (test #\b read-char iport)
   (test #\Return peek-char iport)
   (test 5 input-port-column-number iport)
   (test 1 input-port-line-number iport)
   (test #\Return read-char iport)
   (test #\Linefeed read-char iport)
   (test #\c read-char iport)
   (test #\" read-char iport)
   (test #\Space read-char iport)
   (test #\d read-char iport)
   (test #\Return read-char iport)
   (test 3 input-port-line-number iport)
   (test 1 input-port-column-number iport)
   (test #\k read-char iport)
   (test #\l read-char iport)
   (test #\Linefeed read-char iport)
   (test #\X read-char iport)
   (test #\Y read-char iport)
   (test #!eof read-char iport)))
