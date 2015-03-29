(define sum 0)
(try-finally
      1
      (try-catch
       1
       (ex java.lang.Exception
	   (set! sum 1))))
(format #t "sum: ~d~%" sum)
;; Output: sum: 0
