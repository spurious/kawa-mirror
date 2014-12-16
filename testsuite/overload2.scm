;; Test contributed by Victor van den Elzen
;; for Savannah bug #36973: "poor overload resolution when boxing numerics"

(define-simple-class ContentValues ()
  ((put s::java.lang.String d::double) ::void
   (format #t "called ContentValues(String,double)~%"))
  ((put s::java.lang.String l::long) ::void
   (format #t "called ContentValues(String,long)~%")))


(define (foo cv::ContentValues s::java.lang.String l::long)
  (cv:put s l))

(foo (ContentValues) "hello" 123)
;; Output: called ContentValues(String,long)
