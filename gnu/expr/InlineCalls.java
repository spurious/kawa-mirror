package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.functions.Convert;

public class InlineCalls extends ExpWalker
{
  public static void inlineCalls (Expression exp, Compilation comp)
  {
    InlineCalls walker = new InlineCalls(comp);
    walker.walk(exp);
  }

  public InlineCalls (Compilation comp)
  {
    setContext(comp);
  }

  protected Expression walkApplyExp(ApplyExp exp)
  {
    Expression func = exp.func;

    // Replace (apply (lambda (param ...) body ...) arg ...)
    // by: (let ((param arg) ...) body ...).
    // Note this should be done *before* we walk the lambda, so we can
    // walk the body with params bound to the known args.
    if (func instanceof LambdaExp)
      {
        Expression[] args = exp.args;
        LambdaExp lexp = (LambdaExp) func;
        if (lexp.min_args == lexp.max_args
            && lexp.min_args == args.length
            // Since we re-use the Lambda in-place, we only want to do this
            // for anonymous functions applied directly, as in:
            // (apply (lambda (...) ...) ...)
            // More general function inlining is desirable but more complex.
            && lexp.nameDecl == null)
          {
            LetExp let = new LetExp(args);
            Declaration prev = null;
            int i = 0;
            for (Declaration param = lexp.firstDecl(); param != null; i++)
              {
                Declaration next = param.nextDecl();
                lexp.remove(prev, param);
                let.add(prev, param);
                Expression arg = args[i];
                if ( ! param.getCanWrite())
                  param.setValue(arg);
                prev = param;
                param = next;
              }
            for (Declaration param = let.firstDecl(); param != null; )
              {
                Declaration next = param.nextDecl();
                param = next;
              }
            let.body = lexp.body;
            return walk(let);
          }
      }
    func = walk(func);
    exp.func = func;
    return func.inline(exp, this, null, false);
  }

  /** Walk an ApplyExp assuming function and arguments have been walked. */
  public final Expression walkApplyOnly(ApplyExp exp)
  {
    return exp.func.inline(exp, this, null, true);
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl != null && decl.field == null && ! decl.getCanWrite())
      {
        Expression dval = decl.getValue();
        if (dval instanceof QuoteExp && dval != QuoteExp.undefined_exp)
          return walkQuoteExp((QuoteExp) dval);
        if (dval instanceof ReferenceExp && ! decl.isAlias())
          {
            ReferenceExp rval = (ReferenceExp) dval;
            Declaration rdecl = rval.getBinding();
            Type dtype = decl.getType();
            if (rdecl != null && ! rdecl.getCanWrite()
                && (dtype == null || dtype == Type.pointer_type
                    // We could also allow (some) widening conversions.
                    || dtype == rdecl.getType())
                && ! rval.getDontDereference())
              return walkReferenceExp(rval);
          }
        if (! exp.isProcedureName()
            && ((decl.flags & Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)
                == Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE))
          {
            // FIXME.  This shouldn't be that hard to fix.  For example,
            // we could treat reference to a one-argument method foo as if
            // were (lambda (x) (foo x)).  Or we could treat it as (this):foo.
            // However, it's a little tricky handling the general case.
            // (What about overloading?  Varargs methods?  Static methods?)  
            comp.error('e', "unimplemented: reference to method "+decl.getName()+" as variable");
            comp.error('e', decl, "here is the definition of ", "");
          }
      }
    return super.walkReferenceExp(exp);
  }

  protected Expression walkIfExp (IfExp exp)
  {
    Expression test = exp.test.walk(this);
    if (test instanceof ReferenceExp)
      {
        Declaration decl = ((ReferenceExp) test).getBinding();
        if (decl != null)
          {
            Expression value = decl.getValue();
            if (value instanceof QuoteExp && value != QuoteExp.undefined_exp)
              test = value;
          }
      }
    if (test instanceof QuoteExp)
      {
        return walk(comp.getLanguage().isTrue(((QuoteExp) test).getValue())
                    ? exp.then_clause
                    : exp.else_clause == null ? QuoteExp.voidExp
                    : exp.else_clause);
      }
    exp.test = test;
    if (exitValue == null)
      exp.then_clause = walk(exp.then_clause);
    if (exitValue == null && exp.else_clause != null)
      exp.else_clause = walk(exp.else_clause);
    return exp;
  }

  protected Expression walkLetExp (LetExp exp)
  {
    Declaration decl = exp.firstDecl();
    int n = exp.inits.length;
    for (int i = 0; i < n; i++, decl = decl.nextDecl())
      {
	Expression init0 = exp.inits[i];
	Expression init = walk(init0);
	exp.inits[i] = init;
	Expression dvalue = decl.value;
	if (dvalue == init0)
	  {
	    decl.value = dvalue = init;
	    if (! decl.getFlag(Declaration.TYPE_SPECIFIED))
	      decl.setType(dvalue.getType());
	  }
      }

    if (exitValue == null)
      exp.body = (Expression) walk(exp.body);
    if (exp.body instanceof ReferenceExp)
      {
        ReferenceExp ref = (ReferenceExp) exp.body;
        Declaration d = ref.getBinding();
        if (d != null && d.context == exp && ! ref.getDontDereference())
          {
            if (n == 1)
              {
                Expression init = exp.inits[0];
                Expression texp = d.getTypeExp();
                if (texp != QuoteExp.classObjectExp)
                  init = walkApplyOnly(Convert.makeCoercion(init, texp));
                return init;
              }
            // Can also optimize if n > 1, but have to check if any
            // other inits can cause side-effects.  Probably not worth it.
          }
      }
    return exp;
  }

  protected Expression walkLambdaExp (LambdaExp exp)
  {
    Declaration firstDecl = exp.firstDecl();
    if (firstDecl != null && firstDecl.isThisParameter()
        && ! exp.isClassMethod() && firstDecl.getType() == null)
      {
        firstDecl.setType(comp.mainClass);
      }
    return walkScopeExp(exp);
  }

  protected Expression walkSetExp (SetExp exp)
  {
    super.walkSetExp(exp);
    Declaration decl = exp.getBinding();
    if (! exp.isDefining() && decl != null
        && ((decl.flags & Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)
            == (Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)))
      comp.error('e', "can't assign to method "+decl.getName(), exp);
    /*
    if (decl != null && ! decl.getFlag(Declaration.TYPE_SPECIFIED))
      {
	// This is a kludge to handle the a #!rest parameter that
	// is implicitly declared to be a Scheme <list>, but may be
	// assinged some other value, which is a legal Scheme idiom.
	// We could set implicitly set the parameter type to <list>,
	// but doing so improves type inference in the common case.
	Type declType = decl.getType();
	if (declType != null && ! exp.new_value.getType().isSubtype(declType))
	  decl.setType(Type.pointer_type);
      }
    */
    return exp;
  }
}
