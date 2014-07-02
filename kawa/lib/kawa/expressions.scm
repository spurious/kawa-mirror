;; Experimental API for manipulating and validating expressions.

(module-name kawa.lib.kawa.expressions)
(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)

(define (->exp obj) ::gnu.expr.Expression
  (cond ((gnu.expr.Expression? obj)
         obj)
        ((gnu.expr.Declaration? obj)
         (gnu.expr.ReferenceExp (->gnu.expr.Declaration obj)))
        (else
         (gnu.expr.QuoteExp:getInstance obj))))

(define (get-visitor) ::gnu.expr.InlineCalls
  (gnu.expr.InlineCalls:currentVisitor:get))

(define (visit-exp exp::gnu.expr.Expression
                   #!optional (required ::gnu.bytecode.Type #!null))
  (gnu.expr.ExpVisitor:visit (get-visitor) exp required))

(define (apply-exp func . args)
  (gnu.expr.ApplyExp (->exp func)
                     @(map ->exp args)))

(define-syntax define-validate
  (syntax-rules ()
    ((_ name (exp req proc) clauses ...)
     (define (name
              exp::gnu.expr.ApplyExp
              visitor::gnu.expr.InlineCalls
              required::gnu.bytecode.Type
              proc::gnu.mapping.Procedure) ::gnu.expr.Expression
              (cond clauses ...
                    (else (exp:visitArgs visitor) exp))))))
