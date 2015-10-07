(define-library (ba)
  (import (kawa base))
  (begin
    (let ((a (int[] #:length 10)))
      (format #t "ba body: ~a~%" ["a: " a]))))

(import (ba))
;; Output: ba body: #(a:  [0 0 0 0 0 0 0 0 0 0])
