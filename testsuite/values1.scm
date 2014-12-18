(define (get-values n)
  (case n
    ((0) (values))
    ((1) (values 10))
    ((2) (values 20 21))
    ((3) (values 30 31 32))
    (else (values 99 99))))

(format #t "v2-list: ~w~%"
        (call-with-values (lambda () (get-values 2))
          list))
;; Output: v2-list: (20 21)

(format #t "v3-vec: ~w~%"
        (call-with-values (lambda () (get-values 3))
          vector))
;; Output: v3-vec: #(30 31 32)

(format #t "v2-lam-vec: ~w~%"
        (call-with-values (lambda () (get-values 2))
          (lambda (a b) (vector b a))))
;; Output: v2-lam-vec: #(21 20)

(try-catch
 (call-with-values (lambda () (get-values 3))
   (lambda (a b) (vector b a)))
 (ex java.lang.IndexOutOfBoundsException
     (format #t "v3-lam2-vec: ~w~%" ex)))
;; Output: v3-lam2-vec: java.lang.IndexOutOfBoundsException: too many values

(try-catch
 (call-with-values (lambda () (get-values 3))
   (lambda (a b c d) (vector b a)))
 (ex java.lang.IndexOutOfBoundsException
     (format #t "v3-lam4-vec: ~w~%" ex)))
;; Output: v3-lam4-vec: java.lang.IndexOutOfBoundsException: not enough values
