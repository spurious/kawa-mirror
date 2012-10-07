(format #t "[~a]~%"
        (* 10 (call/cc (lambda (k) (+ 2 (k 3))))))
;; Diagnostic: unreach3.scm:2:36: warning - unreachable procedure call
;; Diagnostic: unreach3.scm:2:41: note - this operand never finishes
;30
;; Output: [30]
