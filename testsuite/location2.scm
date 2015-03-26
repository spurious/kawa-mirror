(define (test-location-local x a)
  (let* ((xl (try-catch (if a
			    (location x)
			    (throw (java.lang.Exception)))
			(ex java.lang.Throwable
			    (throw ex))))
	 (z (xl))
	 (zl (location z)))
    (set! (xl) (+ (zl) 100))
    x))
(format #t "~a~%" (test-location-local 10 0))
;; Output: 110
(try-catch
 (format #t "~a~%" (test-location-local 12 #f))
 (ex java.lang.Throwable
     (format #t "Caught ~a.~%" ex)))
;; Output: Caught java.lang.Exception.
