;; Savannah bug #38890: Wrong more than one applicable method warning
(define (combine-sets (set-i :: <java.util.Set>) (set-j :: <java.util.Set>))
  (if (invoke set-i 'addAll set-j)
    (format (current-output-port) "All good.~%")))
(define hs1 (java.util.TreeSet [5 7 6]))
(combine-sets hs1 (java.util.HashSet [20 19]))
;; Output: All good.
(format (current-output-port) "hs1: ~d~%" hs1)
;; Output: hs1: [5, 6, 7, 19, 20]

;; Savannah bug #38891: Wrong warning with literal double value
(define (test-d)
  (make <java.lang.Double> 3.0))
(format #t "three: ~s~%" (test-d))
;; Output: three: 3.0
