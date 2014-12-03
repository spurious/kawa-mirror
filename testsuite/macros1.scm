;; Complicated macros that used to incorrectly report a cyclic template.

(define-syntax define-labeled-arg-macro
  (syntax-rules ()
    ((define-labeled-arg-macro
       labeled-arg-macro-name
       (positional-macro-name
        (arg-name . arg-def) ...))
     (define-syntax labeled-arg-macro-name
       (syntax-rules ()
         ((labeled-arg-macro-name . kw-val-pairs)
          (let-syntax
              ((find 
                (syntax-rules (arg-name ...)
                  ((find k-args (arg-name . default) arg-name
                         val . others)
                   (next val . k-args)) ...
                   )))
            (next positional-macro-name () 
                  (arg-name . arg-def) ...))))))))

(define-labeled-arg-macro
  make-parser
  (make-parser/positional-args
   (NEW-LEVEL-SEED)
   (PI ())))

(display "ok\n")
;; Output: ok
