(module-name <module1a>)
(module-static #t)

(defmacro define-abc-func (name)
  `(define (,name) 'abc))
