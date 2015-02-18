;; Test references to fields in superclasses,
;; of not-yet-compiled classes.
;; (Alas, references to methods doesn't work yet.)

;; Test subclass before superclass.
(define-simple-class C1 (A1)
  ((bar) (list aval)))

(define-simple-class A1 ()
  (aval ::integer)
  ((toString)::java.lang.String (format "A1[aval:~w]" aval))
  ((foo) aval)
  )
(define-simple-class B1 (A1)
  ((bar) (list aval ));(foo)
  ((toString)::java.lang.String (format "B1[aval:~w]" aval)))

(! b1 (B1 aval: 10))
(format #t "b1:bar->~w~%" (b1:bar))
;; Output: b1:bar->(10)

(define-class C2 (A2)
  ((bar) (list aval ;(foo)
               )))
(define-class A2 ()
  (aval ::integer)
  ;((toString)::java.lang.String (format "A2[aval:~w]" aval))
  ;((foo) aval)
  )
(define-class B2 (A2)
  ((bar) (list aval ));(foo)
  ((toString)::java.lang.String (format "B2[aval:~w]" aval)))

(! b2 (B2 aval: 20))
(format #t "b2:bar->~w~%" (b2:bar))
;; Output: b2:bar->(20)
