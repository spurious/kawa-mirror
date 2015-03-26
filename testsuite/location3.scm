(define (test-location-local x a)
  (let* ((xl (location x))
	 (z (xl))
	 (zl (try-catch (if a
			    (location z)
			    (throw (java.lang.Exception)))
			(ex java.lang.Throwable
			    (throw ex)))))
    (set! (xl) (+ (zl) 100))
    x))
(format #t "~a~%" (test-location-local 10 0))
;; Output: 110
(try-catch
 (format #t "~a~%" (test-location-local 12 #f))
 (ex java.lang.Throwable
     (format #t "Caught ~a.~%" ex)))
;; Output: Caught java.lang.Exception.
