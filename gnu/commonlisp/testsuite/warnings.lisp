(setq x 42)
;; Diagnostic: warnings.lisp:1:1: warning - no declaration seen for x

(write x) (newline)
; TODO: replace Scheme's (newline) with Common Lisp's (terpri)
;; Diagnostic: warnings.lisp:4:8: warning - no declaration seen for x
; FIXME?: this warning (and the additional one below) occurs when
; running with RunTestScript and when running "kawa warnings.lisp",
; but not with "kawa -f warnings.lisp".
;; Output: 42

(defun list-of-numbers (start end)
  (if (> start end)
      nil
    (cons start (list-of-numbers (+ 1 start) end))))
; TODO: use the standard library function #'1+ instead
(defvar list-of-numbers (list-of-numbers 1 3))

(write list-of-numbers) (newline)
;; Output: (1 2 3)

(write (apply #'list-of-numbers '(1 5))) (newline)
;; Output: (1 2 3 4 5)

(let* ((x x)
       (x (cdr (list-of-numbers x (+ x 3)))))
  (write (/ (car (cdr x)) 2)) (newline))
;; Diagnostic: warnings.lisp:25:11: warning - no declaration seen for x
;; Output: 22
