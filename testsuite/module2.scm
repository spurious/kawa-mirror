(module-export list-length-1 list-length-3 classify list1234
	       length-diff1 length-diff2 length-diff3 make-literal make-pair)
(require <module1>)
(require <module1a>)
(define (list-length-1 x) :: <integer>
  (list-length-2 x))
(define list-length-3 #t)
(set! list-length-3 list-length-2)

(define list1234 (deldup (call-to-first '((1 1 2 3 4 4)))))

;; Caused VerifyError
(define name internal-node-name)
(define (classify)
  (let node-loop ((classes '()))
    (map (lambda (class)
	   (lambda (node1)
	     (name node1)))
	 classes)))

;; Based on a bug report from Walter C. Pelissero <walter@pelissero.org>:
(define (length-diff1 (str1 :: <java.lang.String>)
		      (str2 :: <java.lang.String>))
  (let ((diff (- (invoke str1 'length) (invoke str2 'length))))
    diff))
(define (length-diff2 (str1 :: <java.lang.String>)
		      (str2 :: <java.lang.String>))
  (let ((diff :: <int> (- (invoke str1 'length) (invoke str2 'length))))
    diff))
(define (length-diff3 (str1 :: <java.lang.String>)
		      (str2 :: <java.lang.String>)) :: <int>
  (let ((diff :: <int> (- (invoke str1 'length) (invoke str2 'length))))
    diff))

;; Testcase from Jocelyn Paine
(define (make-boolean-literal (boolean <boolean>)) :: <list>
  (list 'boolean boolean))
(define (make-literal value) :: <list>
  (cond
   ((boolean? value) (make-boolean-literal value))
   (else (format #t "Illegal value: ~A~%" value))))

(define (make-pair x y)
  (make <pair> car: x cdr: y))

;; Used to cause stack overflow - see bug #8818.
(define-abc-func abc-returner)
