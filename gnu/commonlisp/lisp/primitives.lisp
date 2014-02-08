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
