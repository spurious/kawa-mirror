(define-library (test export)
  (export fifty-five make-foo foo? foo-x set-foo-x! foo-y)
  (import (scheme base))
  (begin

    (define-syntax fifty-five
      (syntax-rules ()
        ((_) 55)))

    (define-record-type <foo>
      (make-foo x y)
      foo?
      (x foo-x set-foo-x!)
      (y foo-y))

    ))

(import (scheme base)
        (scheme write)
        (test export))

(display (fifty-five))
(newline)
;; Output: 55

(let ((bar (make-foo 3 7)))
  (set-foo-x! bar 5)
  (display (foo-x bar))
  (newline))
;; Output: 5
