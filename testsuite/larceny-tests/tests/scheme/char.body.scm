;;; Included only if the tested implementation claims to support
;;; the unicode feature.

(define (run-char-tests-for-unicode)
    
  (test (char-upcase #\xDF) #\xDF)
  (test (char-downcase #\xDF) #\xDF)
  (test (char-foldcase #\xDF) #\xDF)
    
  (test (char-upcase #\x3A3) #\x3A3)
  (test (char-downcase #\x3A3) #\x3C3)
  (test (char-foldcase #\x3A3) #\x3C3)

  (test (char-upcase #\x3C2) #\x3A3)
  (test (char-downcase #\x3C2) #\x3C2)
  (test (char-foldcase #\x3C2) #\x3C3)

  (test (char-ci=? #\x3C2 #\x3C3) #t)

  (test (char-whitespace? #\x00A0) #t)
  (test (char-upper-case? #\x3A3) #t)
  (test (char-lower-case? #\x3C3) #t)
  (test (char-lower-case? #\x00AA) #t)

  (test (string-upcase "Stra\xDF;e") "STRASSE")
  (test (string-downcase "Stra\xDF;e") "stra\xDF;e")
  (test (string-foldcase "Stra\xDF;e") "strasse")
  (test (string-downcase "\x3A3;") "\x3C3;")

  (test (string-upcase "\x39E;\x391;\x39F;\x3A3;")
        "\x39E;\x391;\x39F;\x3A3;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C2;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3;\x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C3;\x3C2;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3; \x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C2; \x3C3;")
  (test (string-foldcase "\x39E;\x391;\x39F;\x3A3;")
        "\x3BE;\x3B1;\x3BF;\x3C3;")
  (test (string-upcase "\x3BE;\x3B1;\x3BF;\x3C3;")
        "\x39E;\x391;\x39F;\x3A3;") 
  (test (string-upcase "\x3BE;\x3B1;\x3BF;\x3C2;")
        "\x39E;\x391;\x39F;\x3A3;") 

  (test (string-downcase "A\x3A3;'x") "a\x3C3;'x") ; ' is a MidLetter

  (test (string-ci=? "Stra\xDF;e" "Strasse") #t)
  (test (string-ci=? "Stra\xDF;e" "STRASSE") #t)
  (test (string-ci=? "\x39E;\x391;\x39F;\x3A3;" "\x3BE;\x3B1;\x3BF;\x3C2;")
        #t)
  (test (string-ci=? "\x39E;\x391;\x39F;\x3A3;" "\x3BE;\x3B1;\x3BF;\x3C3;")
        #t)

  ;; Systematic testing on every Unicode character.
  ;; The counts are believed to be correct for Unicode 5.0,
  ;; except for char-whitespace? (which has dropped to 25 in Unicode 7.0).
  ;; The counts are likely to increase monotonically (if at all) in later
  ;; versions, but that's not a given.

  (let ((all-count 0)
        (bad-digit-value-count 0)
        (alpha-count 0)
        (numeric-count 0)
        (white-count 0)
        (upper-count 0)
        (lower-count 0))
    (do ((i 0 (+ i 1)))
        ((= i #x110000)
         (test all-count 1112064)
         (test bad-digit-value-count 0)
         (test (<= 93217 alpha-count) #t)
         (test (<= 282 numeric-count) #t)
         (test (<= 25 white-count) #t)
         (test (<= 1362 upper-count) #t)
         (test (<= 1791 lower-count) #t))
      (if (not (<= #xd800 i #xdfff))
          (let* ((c (integer->char i))
                 (n (digit-value c)))
            (if (and (char? c)
                     (char? (char-upcase c))
                     (char? (char-downcase c))
                     (char? (char-foldcase c))
                     (char=? c
                             (integer->char
                              (char->integer c))))
                (set! all-count (+ all-count 1)))
            (if (if (char-numeric? c)
                    (not (and (exact-integer? n) (<= 0 n 9)))
                    n)
                (set! bad-digit-value-count
                      (+ bad-digit-value-count 1)))
            (if (char-alphabetic? c)
                (set! alpha-count (+ alpha-count 1)))
            (if (char-numeric? c)
                (set! numeric-count (+ numeric-count 1)))
            (if (char-whitespace? c)
                (set! white-count (+ white-count 1)))
            (if (char-upper-case? c)
                (set! upper-count (+ upper-count 1)))
            (if (char-lower-case? c)
                (set! lower-count (+ lower-count 1)))))))
  )
