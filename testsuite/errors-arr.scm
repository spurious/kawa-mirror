(define (foo1 x::array2 i::int)
  (x i))
;; Diagnostic: errors-arr.scm:2:3: warning - array has rank 2 but there are 1 indexes

(define (foo2 x::array1[double] i::int)
  (x))
;; Diagnostic: errors-arr.scm:6:3: warning - array has rank 1 but there are 0 indexes

(define (foo3 x::array2[integer] i::int j::int)::string
  (x i j))
;; Diagnostic: errors-arr.scm:10:3: warning - type integer is incompatible with required type string

(define (foo4 x::array2[double] i::int j::int)::string
  (x i j))
;; Diagnostic: errors-arr.scm:14:3: warning - type double is incompatible with required type string
