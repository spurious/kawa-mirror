(define (stringp x) <elisp:boolean>
  (instance? x <string>))

(define (make-string (count :: <int>) ch)
  (make <string> count (invoke-static <gnu.jemacs.lang.ELisp> 'asChar ch)))

