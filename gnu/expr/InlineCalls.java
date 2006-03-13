package gnu.expr;
import gnu.mapping.*;
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
    exp.walkChildren(this);
    Expression test = exp.test;
    if (test instanceof QuoteExp)
      {
	Language language = comp.getLanguage();
	if (language.isTrue(((QuoteExp) test).getValue()))
	  return exp.then_clause;
	else
	  return exp.else_clause == null ? QuoteExp.voidExp : exp.else_clause;
      }
    return exp;
  }

  protected Expression walkLetExp (LetExp exp)
  {
    Declaration decl = exp.firstDecl();
    for (int i = 0; i < exp.inits.length; i++, decl = decl.nextDecl())
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
