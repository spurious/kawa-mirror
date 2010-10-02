;; Savannah bug #31180: exception in inline-compiler
;; Note this only failed in immediate mode, with foo exported but bar private.
(module-export foo)
(module-static #t)
(define (bar (port-number <int>) name)
  (list port-number name))

(define (foo port name) (bar port name))

;;Output: (5 foo)
