;(load "/home/bothner/kawa-bin/testsuite/testing.zip")
(test-init "elisp language" 3)

(setq y 100)
(defun foo1 (x)
  (+ x y))
(defun foo2 (y)
  (foo1 30))
(test 50 'fluid-test-1 (foo2 20))

;(test t 'eq-test-1 (eq t 't))

(test t 'equal-test-1 (equal "tt" "tt"))
(test nil 'equal-test-1 (equal "tt" "tt "))

