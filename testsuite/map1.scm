;; Test map and related functions.

(define (generic-map-format seq)
  (map (lambda (x) (format "{~w}" x))
       seq))

(define (generic-vmap-format seq)
  (vector-map (lambda (x) (format "{~w}" x))
       seq))

(define (write-list prefix lst::list)
  (format #t "~a~{~a~^, ~}~%" prefix lst))

(define (write-vector prefix lst::vector)
  (format #t "~a~{~a~^, ~}~%" prefix lst))

(define (list-map-format seq::list)
  (map (lambda (x) (format "{~w}" x)) seq))
(! list1 (list 3 #!eof 5 "abc" #(a bb)))
(write-list "list1a: " (generic-map-format list1))
;; Output: list1a: {3}, {#!eof}, {5}, {"abc"}, {#(a bb)}
(write-list "list1b: " (list-map-format list1))
;; Output: list1b: {3}, {#!eof}, {5}, {"abc"}, {#(a bb)}
(write-vector "list1c: " (generic-vmap-format list1))
;; Output: list1c: {3}, {#!eof}, {5}, {"abc"}, {#(a bb)}

(define (string-map-format seq::string)
  (map (lambda (x) (format "{~w}" x)) seq))
(! string1 "😂abc😂c😂")
(write-list "string1a: " (generic-map-format string1))
;; Output: string1a: {#\x1f602}, {#\a}, {#\b}, {#\c}, {#\x1f602}, {#\c}, {#\x1f602}
(write-list "string1b: " (string-map-format string1))
;; Output: string1b: {#\x1f602}, {#\a}, {#\b}, {#\c}, {#\x1f602}, {#\c}, {#\x1f602}

(define (char-array-map-format seq::char[])
  (map (lambda (x) (format "{~w}" x)) seq))
(! chars1 ::char[] (string1:toCharArray))
(write-list "chars1a: " (generic-map-format chars1))
;; Output: chars1a: {#\xd83d}, {#\xde02}, {#\a}, {#\b}, {#\c}, {#\xd83d}, {#\xde02}, {#\c}, {#\xd83d}, {#\xde02}
(write-list "chars1b: " (char-array-map-format chars1))
;; Output: chars1b: {#\xd83d}, {#\xde02}, {#\a}, {#\b}, {#\c}, {#\xd83d}, {#\xde02}, {#\c}, {#\xd83d}, {#\xde02}

(define (vector1-vmap-format v1::vector)
  (vector-map (lambda (x) (format "{~w}" x))
              v1))
(define (vector2-vmap-format v1::vector v2::vector)
  (vector-map (lambda (x y) (format "{~w;~w}" x y))
              v1 v2))
(! vec1 #(2 a 8 b))
(! vec2 [10 "AB" 30])
(write-list "vec1a: " (generic-map-format vec1))
;; Output: vec1a: {2}, {a}, {8}, {b}
(write-vector "vec1b: " (generic-vmap-format vec1))
;; Output: vec1b: {2}, {a}, {8}, {b}
(write-vector "vec1c: " (vector1-vmap-format vec1))
;; Output: vec1c: {2}, {a}, {8}, {b}
(write-vector "vec2a: " (vector2-vmap-format vec1 vec2))
;; Output: vec2a: {2;10}, {a;"AB"}, {8;30}
