(require <kawa.lib.prim_syntax>)

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

;; The following functions are written so they will work and generate
;; good code for both Java5 (which has Character.toUppercase(int))
;; and older Java versions (which only have Character.toUppercase(char)).
(define (char-ci=? (c1 :: <character>) (c2 :: <character>)) :: <boolean>
  (= (as <int> (java.lang.Character:toUpperCase (invoke c1 'intValue)))
     (as <int> (java.lang.Character:toUpperCase (invoke c2 'intValue)))))
(define (char-ci<? (c1 :: <character>) (c2 :: <character>)) :: <boolean>
  (< (as <int> (java.lang.Character:toUpperCase (invoke c1 'intValue)))
     (as <int> (java.lang.Character:toUpperCase (invoke c2 'intValue)))))
(define (char-ci>? (c1 :: <character>) (c2 :: <character>)) :: <boolean>
  (> (as <int> (java.lang.Character:toUpperCase (invoke c1 'intValue)))
     (as <int> (java.lang.Character:toUpperCase (invoke c2 'intValue)))))
(define (char-ci<=? (c1 :: <character>) (c2 :: <character>)) :: <boolean>
  (<= (as <int> (java.lang.Character:toUpperCase (invoke c1 'intValue)))
     (as <int> (java.lang.Character:toUpperCase (invoke c2 'intValue)))))
(define (char-ci>=? (c1 :: <character>) (c2 :: <character>)) :: <boolean>
  (>= (as <int> (java.lang.Character:toUpperCase (invoke c1 'intValue)))
     (as <int> (java.lang.Character:toUpperCase (invoke c2 'intValue)))))
