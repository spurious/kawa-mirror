;(module-name <cycle1>)
(module-export is-even?)
(require <cycle2>)

(define one :: <int> -2)
; Verifies that body is executed exactly once.
(set! one (+ one 3))

(define (is-even? (x :: <int>)) :: <boolean>
  (if (= x 0) #t (is-odd? (- x one))))


