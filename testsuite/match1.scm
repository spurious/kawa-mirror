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
