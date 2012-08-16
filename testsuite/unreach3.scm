(format #t "[~a]~%"
        (* 10 (call/cc (lambda (k) (+ 2 (k 3))))))
;; Diagnostic: unreach3.scm:2:15: warning - unreachable code
;30
;; Output: [30]
