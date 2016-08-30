;; Savannah bug #48938: lambda cannot be cast to java.util.Comparator

(define-syntax ->
  (syntax-rules ()
     ((_ o)
      o)
     ((_ o (m a ...) r ...)
      (-> (invoke o 'm a ...) r ...))
     ((_ o f r ...)
      (-> (field o 'f) r ...))))

(format #t "out:~w~%~!"
(-> [1 3 5 7 9 8 6 4 2]
   (stream)
   (filter (lambda (x) (<= x 5)))
   (sorted (lambda (a b) (- b a)))
   (toArray)))

;; Output: out:5
