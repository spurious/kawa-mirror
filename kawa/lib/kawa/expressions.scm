;; Experimental API for manipulating and validating expressions.

(module-name (kawa expressions))
(export ->exp get-visitor get-compilation visit-exp
        syntax-as-exp define-validate
        apply-exp begin-exp if-exp set-exp
        Declaration Expression ApplyExp QuoteExp Compilation Type)
(define-alias Expression gnu.expr.Expression)
(define-alias ApplyExp gnu.expr.ApplyExp)
(define-alias QuoteExp gnu.expr.QuoteExp)
(define-alias Compilation gnu.expr.Compilation)
(define-alias Declaration gnu.expr.Declaration)
(define-alias Type gnu.bytecode.Type)

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)

(define (->exp obj) ::Expression
  (cond ((gnu.expr.Expression? obj)
         obj)
        ((gnu.expr.Declaration? obj)
         (gnu.expr.ReferenceExp (->Declaration obj)))
        (else
         (gnu.expr.QuoteExp:getInstance obj))))

(define (get-visitor) ::gnu.expr.InlineCalls
  (gnu.expr.InlineCalls:currentVisitor:get))

(define (get-compilation) ::gnu.expr.Compilation
  ((get-visitor):getCompilation))

(define (visit-exp exp::gnu.expr.Expression
                   #!optional (required ::gnu.bytecode.Type #!null))
  ::gnu.expr.Expression
  ((get-visitor):visit exp required))

(define-syntax syntax-as-exp
  (lambda (form)
    (syntax-case form ()
      ((_ expr)
       (syntax->expression (syntax expr))))))

(define (apply-exp func . args) ::gnu.expr.ApplyExp
  (gnu.expr.ApplyExp (->exp func)
                     @(gnu.kawa.functions.Map:map1 ->exp args)))

(define (begin-exp . args)
  (gnu.expr.BeginExp @(gnu.kawa.functions.Map:map1 ->exp args)))

(define (if-exp a b #!optional (c #!null))
  (gnu.expr.IfExp (->exp a) (->exp b) (if (eq? c #!null) c (->exp c))))

(define (set-exp (var::gnu.expr.Declaration) val)
  (let ((se (gnu.expr.SetExp var (->exp val))))
    (se:setContextDecl var)
    (var:setCanWrite #t)
    (se:setBinding var)
    (var:noteValueFromSet se)
    se))

(define-syntax define-validate
  (syntax-rules ()
    ((_ name (exp req proc) clauses ...)
     (define (name
              exp::gnu.expr.ApplyExp
              visitor::gnu.expr.InlineCalls
              required::gnu.bytecode.Type
              proc::gnu.mapping.Procedure) ::gnu.expr.Expression
              (let ((ex ::gnu.expr.Expression
                        (cond clauses ... (else #!null))))
                (cond ((eq? ex #!null)
                       #!null)
                      ((eq? ex exp)
                       (exp:visitArgs visitor)
                       exp)
                      (else
                       (visitor:visit (ex:maybeSetLine exp) required))))))))
