(define (test-location-local x)
  (let ((xl (location x)))
    (let ((z (xl)))
      (let* ((zr (object ()
                         ((doit)
                          (location z))))
             (zl (zr:doit)))
 	(set! (xl) (+ (zl) 100))
	x))))

(format #t "test-location-local 10: ~w~%"
        (test-location-local 10))
;; Output: test-location-local 10: 110
