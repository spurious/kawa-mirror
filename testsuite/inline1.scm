#! /bin/sh
#| -*- scheme -*-
exec /usr/local/bin/kawa $0 "$@"
|#

(define-library (fuh)
  (export gunzip-genport)
  (import (scheme base))
  (begin

    (define (gunzip-genport)

      (define bk 0) 

      (define (inflate-codes bl)
        (set! bk (+ bk 8)))

      (define (inflate-dynamic)
        (inflate-codes 7))

      (let loop ()
        (inflate-dynamic)
        (let laap ((res (vector 'return #f)))
          (let ((state (vector-ref res 0))
                (r (vector-ref res 1)))
            (case state
              ((return)
               (if r (loop) #f))
              ((flush)
               (lambda () (laap #f))))))))))

(import (scheme base)
        (fuh))
(gunzip-genport)
(format #t "Done.~%")
;; Output: Done.
