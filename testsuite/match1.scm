(format #t "to string: ~w~%"
        (map (lambda (x) (if (? s ::string x) s "N/A")) '(3 "hello" 3.5 #\?)))
;; Output: to string: ("N/A" "hello" "N/A" "N/A")

(format #t "to String: ~w~%"
        (map (lambda (x) (if (? s ::String x) s "N/A")) '(3 "hello" 3.5 #\?)))
;; Output: to String: (3 "hello" 3.5 #\?)

(format #t "to character: ~w~%"
        (map (lambda (x) (if (? c ::character x) c #\?))
             (list #!eof 3 "hello" #\X (java.lang.Character #\Y))))
;; Output: to character: (#\? #\? #\? #\X #\Y)

(format #t "to character-or-eof: ~w~%"
        (map (lambda (x) (if (? c ::character-or-eof x) c #\?))
             (list #!eof 3 "hello" #\X (java.lang.Character #\Y))))
;; Output: to character-or-eof: (#!eof #\? #\? #\X #\Y)

(format #t "to char: ~w~%"
        (map (lambda (x) (if (? c ::char x) c #\?))
             (list #!eof 3  #\X (java.lang.Character #\Y) #\x12345)))
;; Output: to char: (#\? #\? #\X #\Y #\?)

(format #t "to boolean: ~w~%"
        (map (lambda (x) (if (? b::boolean x) b -1)) '(1 #f #\X #t)))
;; Output: to boolean: (-1 #f -1 #t)

(define (to-v o)
  (if (? v ::vector o) v '#()))
(format #t "to vector: ~w~%"
        (map to-v '(4 (5) #(6))))
;; Output: to vector: (#() #() #(6))

(define (to-f64v o)
  (if (? f64v ::f64vector o) f64v '#f64(-1 0)))
(format #t "to f64v: ~w~%"
        (map to-f64v '(4 #(5) #f64(7))))
;; Output: to f64v: (#f64(-1.0 0.0) #f64(-1.0 0.0) #f64(7.0))

(define (to-list o)
  (if (? l ::list o) l '()))
(format #t "to list: ~w~%"
        (map to-list '(4 #(5 6) (7 6))))
;; Output: to list: (() () (7 6))

(define (to-FVector o)
  (if (? v ::gnu.lists.FVector o) v '#()))
(format #t "to FVector: ~w~%"
        (map to-FVector '(4 (5) #(6))))
;; Output: to FVector: (#() #() #(6))

(define (to-F64Vector o)
  (if (? f64v ::gnu.lists.F64Vector o) f64v '#f64(1 2)))
(format #t "to F64Vector: ~w~%"
        (map to-f64v '(4 #(5) #f64(7))))
;; Output: to F64Vector: (#f64(-1.0 0.0) #f64(-1.0 0.0) #f64(7.0))

(define (to-LList o)
  (if (? l ::gnu.lists.LList o) l '()))
(format #t "to LList: ~w~%"
        (map to-LList '(4 #(5 6) (7 6))))
;; Output: to LList: (() () (7 6))
