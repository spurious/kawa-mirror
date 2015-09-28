;; Test forward reference in macro.

(define-syntax do-hello
  (lambda (stx)
    (hello (syntax stx))))

(define (hello stx)
  "Hello")

(format #t "Result ~w~%~!" (do-hello (foo bar)))
;; Output: Result "Hello"
