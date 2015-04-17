(try-catch
 (try-catch
  (primitive-throw (java.lang.Error "my-error"))
  (ex1 java.lang.Throwable
       (let ((cf (lambda (cv)
                   (list ex1))))
         (format #t "ex1:~w cf:~w~%~!" ex1 cf)
         (primitive-throw ex1))))
 (ex2 java.lang.Throwable
      (format #t "caught ~w~%" ex2)))

;; Output: ex1:java.lang.Error: my-error cf:#<procedure cf>
;; Output: caught java.lang.Error: my-error
