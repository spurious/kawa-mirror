package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.Invoke;

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
    super.walkApplyExp(exp);
    return walkApplyOnly(exp);
  }

  /** Walk an ApplyExp assuming function and arguments have been walked. */
  public Expression walkApplyOnly(ApplyExp exp)
  {
    return exp.func.inline(exp, this, null);
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl != null && decl.getFlag(Declaration.IS_CONSTANT)
        && decl.field == null)
      {
	Expression dval = decl.getValue();
        if (dval instanceof QuoteExp && dval != QuoteExp.undefined_exp)
          return walkQuoteExp((QuoteExp) dval);
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
        if (d.context == exp && ! ref.getDontDereference())
          {
            if ( n == 1)
              return exp.inits[0];
            // Can also optimize of n > 1, but have to check if any
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

  /*
  protected Expression walkSetExp (SetExp exp)
  {
    super.walkExp(exp);
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
    return exp;
  }
  */
}
