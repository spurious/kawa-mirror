(defun car (x)
  (if (null x)
      nil
      (invoke (the pair x) '|getCar|)))

(defun first (x)
  (car x))

(defun cdr (x)
  (if (null x)
      nil
      (invoke (the pair x) '|getCdr|)))

(defun rest (x)
  (cdr x))

(defun second (x)
  (first (rest x)))

(defun third (x)
  (first (rest (rest x))))

(defun nthcdr (n lst)
  (declare (int n))
  (do ((i n (1- i))
       (result lst (cdr result)))
      ((= i 0) result)))

(defun nth (n x)
  (first (nthcdr n x)))

(defun 1- (x) (- x 1))
(defun 1+ (x) (+ x 1))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun listp (obj)
  (typep obj 'list))

(defun numberp (obj)
  (typep obj 'number))

(defun atom (obj)
  (not (consp obj)))

(defun eql (x y)
  (eqv? x y))

(defun complement (pred)
  (lambda (&rest arguments)
    (not (apply pred arguments))))

(defun member-with-test (x lst test key)
  (declare (list lst))
  (cond ((null lst) nil)
	((funcall test x (funcall key (car lst))) lst)
	(t (member-with-test x (cdr lst) test key))))

(defun member-with-key (x lst key)
  (declare (list lst))
  (cond ((null lst) nil)
	((eql x (funcall key (car lst))) lst)
	(t (member-with-key x (cdr lst) key))))

(defun member-plain (x lst)
  (declare (list lst))
  (cond ((null lst) nil)
	((eql x (car lst)) lst)
	(t (member-plain x (cdr lst)))))

(defun member (x lst &key key
		       (test nil test-supplied)
		       (test-not nil test-not-supplied))
  (declare (list lst))
  (cond (test-supplied
	 (member-with-test x lst test key))
	(test-not-supplied
	 (member-with-test x lst (complement test-not) key))
	(key
	 (member-with-key x lst key))
	(t
	 (member-plain x lst))))

(defun apply (func &rest args)
  (invoke (the |function|
	       (if (symbolp func)
		   (symbol-function func)
		   func))
	  '|applyN|
	  (invoke-static |gnu.kawa.functions.Apply|
			 '|getArguments|
			 args
			 0 #'apply)))
