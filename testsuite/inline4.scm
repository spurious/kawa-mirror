;; This used to fail because g would get inlined into f, and h into g.
;; Then we set up the local-variable and stack-map attributes so that x2
;; is defined (and has type int) in h (because the inlining causes it
;; to be logically nested in g).  However, x2 is never set if we call h
;; directly from f, so we get a conflict.
;; This fix was to defer te inlining, rather than doing it in-place.

(format #t "r:~w~%"
        (letrec ((f (lambda (x1::int)
                      (if (eqv? x1 1)
                          (g x1)
                          (h x1))))
                 (g (lambda (x2::int)
                      (h x2)))
                 (h (lambda (x3::int)
                      (case x3
                        ((0) (h x3))
                        ((2) x3)
                        (else 123)
                        )
)))
          (f 45)))
;; Output: r:123
