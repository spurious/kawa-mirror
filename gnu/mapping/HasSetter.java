package gnu.mapping;

/** A Procedure that can be used on the left-and-side of an assignment.
  * It can do that by overriding getSetter.
  * Alternatively, it can use the default getSetter, as long as it
  * overrides one of set0 (if a 0-argument Procedure)
  * set1 (if a 1-argument Procedure), or setN (otherwise).
  */

public interface HasSetter
{
  public Procedure getSetter();
};

/*
(define-syntax set!
 (syntax-rules ()
  ((set! (func arg ...) rhs)
   ((setter func) rhs arg ...))
  ((set! var rhs)
   (%set! var rhs))))

Better code generation:

(apply (setter f) rhs args ...)
=> f.set(rhs args ...)
E.g.
(set (f) rhs) =>  [compile f].set0([compile rhs])
(set (f x) rhs) =>  [compile f].set1([compile rhs], [compile x])
(set (f x1 .. xn) rhs) => [compile f].setN([compile rhs], .. [compile xn])

Examples:
(setter car) == (lambda (rhs pair) (set-car! pair rhs))
(setter cdr) == (lambda (rhs pair) (set-cdr! pair rhs))
(setter caXXXr) == (lambda (rhs pair) (set-car! (cXXXr pair) rhs))
(setter cdXXXr) == (lambda (rhs pair) (set-cdr! (cXXXr pair) rhs))
(setter list-ref) ...

(ENV 'NAME) => value of NAME in ENV
(set! (ENV 'NAME) VAL) => set value of NAME in ENV to VAL

(CELL) => current value of CELL
(set! (CELL) VALUE) => set value of CELL to VALUE
*/
