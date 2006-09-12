(module-name <cycle2>)
(module-export is-odd?)

(require <cycle1>)

(define one :: <int> 0)
; Verifies that body is executed exactly once.
(set! one (+ one 1))

(define (is-odd? (x :: <int>)) :: <boolean>
  (if (= x 0) #f (is-even? (- x one))))
