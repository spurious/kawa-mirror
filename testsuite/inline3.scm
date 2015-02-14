(define (split-by-charset str delimeters maxsplit)
  (define (scan-beg-word) (scan-word 0 #f #f))
  (define (scan-word i from yet-to-split-count)
    (cond
     ((>= i 0) #f)
     ((memq (string-ref str i) '(#\a))
      (cons "" (scan-beg-word)))
     (else (scan-word i from yet-to-split-count))
     ))
  (scan-beg-word))

(let ((charset (car '(1 2)))
      (maxsplit (if (pair? '()) 0 0)))
  (split-by-charset "" charset maxsplit))

(format #t "Ok.~%")
;; Output: Ok.
