(let ((x ::int 1))
  (set! x 0)
  (try-finally
   (if x (display "x is true")
       (display "x is not true"))
   (format #t "~%finally-clause executed~%")))
;; Output: x is true
;; Output: finally-clause executed
