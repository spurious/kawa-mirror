(defmacro whenx (cond exp . rest)
  `(if ,cond
       (begin ,exp . ,rest)))
(define-for-syntax (alter-syntax-datum proc stx)
  (datum->syntax-object stx (proc (syntax-object->datum stx))))
(define-syntax define-symbol-altering-macro
  (syntax-rules ()
    ((_ (macro-name arg) expr . exprs)
     (define-symbol-altering-macro macro-name (lambda (arg) expr . exprs)))
    ((_ macro-name proc)
     (define-syntax macro-name
       (lambda (stx)
         (syntax-case stx ()
           ((_ sym . args)
            (let ((new-sym (alter-syntax-datum proc (syntax sym))))
              #`(#,new-sym . args)))))))))
(define-symbol-altering-macro (call-reversename sym)
  (string->symbol ;; OK: lambda2 BAD: lambda3
   (list->string
    (reverse
     (string->list
      (symbol->string sym))))))
(display (call-reversename xam 3 2 7 6)) (newline)
;; Output: 7
