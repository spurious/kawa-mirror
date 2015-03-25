(try-catch
 ((lambda ()
    (try-finally
     1
     (throw (java.lang.Exception)))))
 (ex java.lang.Throwable
     (format #t "Caught ~a.~%" ex)))
;; Output: Caught java.lang.Exception.
