(define (char? x)
  (instance? x <character>))

(define (char-alphabetic? (char <char>))
  (invoke-static <java.lang.Character> "isLetter" char))

(define (char-numeric? (char <char>))
  (invoke-static <java.lang.Character> "isDigit" char))

(define (char-whitespace? (char <char>))
  (invoke-static <java.lang.Character> "isSpace" char))

(define (char-upper-case? (char <char>))
  (invoke-static <java.lang.Character> "isUpperCase" char))

(define (char-lower-case? (char <char>))
  (invoke-static <java.lang.Character> "isLowerCase" char))

(define (char->integer (char <character>))
  (invoke char 'intValue))

(define (integer->char (n <int>))
  (invoke-static <character> 'make n))

(define (char-upcase (char <char>))
  (invoke-static <java.lang.Character> "toUpperCase" char))

(define (char-downcase (char <char>))
  (invoke-static <java.lang.Character> "toLowerCase" char))

(define (char=? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Eq c1 c2))
(define (char<? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Ls c1 c2))
(define (char>? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Gr c1 c2))
(define (char<=? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Ls$Eq c1 c2))
(define (char>=? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Gr$Eq c1 c2))

(define (char-ci=? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Eq
                 (invoke-static <java.lang.Character> 'toUpperCase c1)
                 (invoke-static <java.lang.Character> 'toUpperCase c2)))
(define (char-ci<? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Ls
                 (invoke-static <java.lang.Character> 'toUpperCase c1)
                 (invoke-static <java.lang.Character> 'toUpperCase c2)))
(define (char-ci>? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Gr
                 (invoke-static <java.lang.Character> 'toUpperCase c1)
                 (invoke-static <java.lang.Character> 'toUpperCase c2)))
(define (char-ci<=? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Ls$Eq
                 (invoke-static <java.lang.Character> 'toUpperCase c1)
                 (invoke-static <java.lang.Character> 'toUpperCase c2)))
(define (char-ci>=? (c1 :: <character>) (c2 :: <character>))
  (invoke-static <character> '$Gr$Eq
                 (invoke-static <java.lang.Character> 'toUpperCase c1)
                 (invoke-static <java.lang.Character> 'toUpperCase c2)))

