(TEST-INIT "macros")

(test 'ok 'letxx (let ((xx #f)) (cond (#t xx 'ok))))

;;; FIXME this does not work - hygiene problem!
;;;(test 'ok 'let=> (let ((=> #f)) (cond (#t => 'ok))))

(load (string-append src-prefix "mac1.scm"))
(test '(1 2) 'something (something 1 2))

(test '(2 3) 'something (something 2 3))
