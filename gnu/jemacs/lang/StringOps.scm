(define (stringp x) <elisp:boolean>
  (instance? x <string>))

(define (make-string (count :: <int>) ch)
  (make <string> count (invoke-static <gnu.jemacs.lang.ELisp> 'asChar ch)))

(define (substring (str :: <string>) from #!optional (to '()))
  (if (eq? to '())
      (set! to (string-length str)))
  (if (< to 0)
      (set! to (- (string-length str) to)))
  (if (< from 0)
      (set! from (- (string-length str) from)))
  (invoke str 'copy from to))

(define (char-to-string ch)
  (make <string> 1 (invoke-static <gnu.jemacs.lang.ELisp> 'asChar ch)))

