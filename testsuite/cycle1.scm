(module-name <cycle1>)
(module-export is-even? cycle1-name1 cycle1-name2 cycle1-name3)
(require "cycle2.scm")

;; Check that c1, c2, and <cycle1> all evaluate to java.lang.Class objects.
(define-alias c1 <cycle1>)
(define-namespace c2 <cycle1>)
(define (cycle1-name1) c1)
(define (cycle1-name2) c2)
(define (cycle1-name3) <cycle1>)

(define one :: <int> -2)
; Verifies that body is executed exactly once.
(set! one (+ one 3))

(define (is-even? (x :: <int>)) :: <boolean>
  (if (= x 0) #t (is-odd? (- x one))))
