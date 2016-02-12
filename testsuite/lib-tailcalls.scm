;; Kawa-options: "--full-tailcalls" %F
;;; Based on testcase from OKUMURA Yuki <mjt@cltn.org> reported 2016-02-11

(define-library (minidispatch)
  (export make-minidispatch-obj minitype-set!)
  (import (scheme base) (scheme write))
  (begin

    (define (minitype-set! obj::vector slot::int value)
      (display (list 'SET!-CALLED: slot value))(newline)
      (vector-set! obj slot value))

    (define (baseset! obj::vector slot::int v)
      (minitype-set! obj slot v))

    (define (make-minidispatch-obj class param)
      (let ((obj (vector "UNINITIALIZED_x" "UNINITIALIZED_y")))
        (baseset! obj 0 class) ;; This gets skipped
        (baseset! obj 1 param)
        obj))
))

(import (scheme base)
        (minidispatch))

(let ((obj0 (make-minidispatch-obj (vector 'testa 'testb) "INIT12")))
  (write obj0)
  (newline))

;; Output: (SET!-CALLED: 0 #(testa testb))
;; Output: (SET!-CALLED: 1 INIT12)
;; Output: #(#(testa testb) "INIT12")
