(require <kawa.lib.prim_syntax>)

(define (char? x)
  (instance? x <character>))

(define (char->integer (char <character>))
  (invoke char 'intValue))

(define (integer->char (n <int>))
  (invoke-static <character> 'make n))

(define (char=? (c1 :: <character>) (c2 :: <character>)) :: <boolean>
  (= (invoke c1 'intValue) (invoke c2 'intValue)))
(define (char<? (c1 :: <character>) (c2 :: <character>))
  (< (invoke c1 'intValue) (invoke c2 'intValue)))
(define (char>? (c1 :: <character>) (c2 :: <character>))
  (> (invoke c1 'intValue) (invoke c2 'intValue)))
(define (char<=? (c1 :: <character>) (c2 :: <character>))
  (<= (invoke c1 'intValue) (invoke c2 'intValue)))
(define (char>=? (c1 :: <character>) (c2 :: <character>))
  (>= (invoke c1 'intValue) (invoke c2 'intValue)))

(define (digit-value ch::character)
  (let ((r (java.lang.Character:digit (ch:intValue) (->int 10))))
    (if (< r 0) #f (->integer r))))
