(test-init "Common Lisp tests" 11)

(setq y 100)
(defun foo1 (x)
  (lambda (y)
    (/ x y)))
(defvar foo12 (foo1 12))
(test 4 'lexical-test-1 (apply foo12 '(3)))

(defvar xx 20)
(defun xx (a) (+ xx a))
(test 24 'separate-names-1 (xx 4))

;(test t 'eq-test-1 (eq t 't))

(test t 'equal-test-1 (equal "tt" "tt"))
(test nil 'equal-test-2 (equal "tt" "tt "))

(test "The octal value of 18 is 22,
   and the hex value is 12."
      'format-test-1
      (format "The octal value of ~d is ~o,
   and the hex value is ~x." 18 18 18))

(defmacro list-1 (bar) `(list ,bar))
(test '(12) 'defmacro (list-1 12))

(test '(3 4) 'list-1 (list 3 4))
(test nil 'functionp-1 (functionp 'list))
(test t 'functionp-2 (functionp #'list))
(test t 'functionp-3 (functionp (function list)))
(test '(3 4) 'function-1 ((function list) 3 4))
