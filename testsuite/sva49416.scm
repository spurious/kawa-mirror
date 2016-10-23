;; Savannah bug #49416 "Error macro-expanding with-syntax with ... patterns"
;; Contributed by AdriánMedraño Calvo

(define-library (somemodule)
  (import
    (scheme base)
    (kawa base)
    (kawa lib std_syntax))
  (begin
    (define-syntax some-list
      (lambda (stx)
        (syntax-case stx ()
          ((_ a b rest ...)
           (with-syntax (((thelist ...) (syntax (1 2 3))))
             (syntax (list thelist ...)))))))

    (display 3)
    (display (some-list
              aaa
              bbb
              (cons 3 222)
              (cons 2 333)))))

(import (somemodule))
(newline)
;; Output: 3 (1 2 3)
