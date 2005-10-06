package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.ClassMethodProc;

public class InlineCalls extends ExpWalker
{
  public static void inlineCalls (Expression exp, Compilation comp)
  {
    InlineCalls walker = new InlineCalls();
    walker.setContext(comp);
    walker.walk(exp);
  }

  protected Expression walkApplyExp(ApplyExp exp)
  {
    if (comp.inlineOk(exp.func))
      exp = ClassMethodProc.rewrite(exp);
    super.walkApplyExp(exp);
    LambdaExp lambda = null;
    int nargs = exp.getArgCount();
    if (exp.func instanceof LambdaExp)
      lambda = (LambdaExp) exp.func;
    Expression func = exp.func;
    Declaration decl = null;
    if (func instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) func;
        decl = rexp.binding;
        if (decl != null && ! decl.getFlag(Declaration.IS_UNKNOWN))
	  {
	    decl = Declaration.followAliases(decl);
	    if (decl.isIndirectBinding())
	      return exp;
            func = decl.getValue();
	    if (func instanceof LambdaExp) 
	      lambda = (LambdaExp) func;
	  }
	else if (rexp.getSymbol() instanceof Symbol)
	  {
	    Symbol symbol = (Symbol) rexp.getSymbol();
	    Object fval = Environment.getCurrent().getFunction(symbol, null);
	    if (fval instanceof Procedure)
	      func = new QuoteExp(fval);
	    decl = null;
	  }
      }
    if (func instanceof QuoteExp && func != QuoteExp.undefined_exp)
      {
	Object fval = ((QuoteExp) func).getValue();
	if (! (fval instanceof Procedure))
	  return noteError(decl == null || fval == null ? "called value is not a procedure"
			   : ("calling " + decl.getName()
			      + " which is a "+fval.getClass().getName()));
	Procedure proc = (Procedure) fval;
	String msg = WrongArguments.checkArgCount(proc, nargs);
	if (msg != null)
	  return noteError(msg);
	if (proc instanceof CanInline)
	  return ((CanInline) proc).inline(exp, this);
	if (exp.getFlag(ApplyExp.INLINE_IF_CONSTANT))
	  {
	    Expression e = exp.inlineIfConstant(proc, this);
	    if (e != exp)
	      return walk(e);
	  }
	if (comp.inlineOk(proc))
	  {
	    if (proc instanceof Inlineable)
	      return new ApplyExp(proc, exp.getArgs()).setLine(exp);
	    PrimProcedure mproc
	      = PrimProcedure.getMethodFor(proc, decl, exp.args,
					   comp.getLanguage());
	    if (mproc != null)
	      {
		ApplyExp nexp;
		if (mproc.getStaticFlag() || decl == null)
		  nexp = new ApplyExp(mproc, exp.args);
		else if (decl.base == null)
		  return exp;
		else
		  {
		    Expression[] margs = new Expression[1 + nargs];
		    System.arraycopy(exp.getArgs(), 0, margs, 1, nargs);
		    margs[0] = new ReferenceExp(decl.base);
		    nexp = new ApplyExp(mproc, margs);
		  }
		return nexp.setLine(exp);
	      }
	  }
      }
    if (lambda != null)
      {
	int args_length = exp.args.length;
	String msg = WrongArguments.checkArgCount(lambda.getName(),
						  lambda.min_args,
						  lambda.max_args,
						  args_length);
	if (msg != null)
	  return noteError(msg);

        int conv = lambda.getCallConvention();
        Method method;
	if (comp.inlineOk(lambda)  && lambda.isClassMethod()
	    && (conv <= Compilation.CALL_WITH_CONSUMER
		|| (conv == Compilation.CALL_WITH_TAILCALLS))
	    && (method = lambda.getMethod(args_length)) != null)
	  {
            // This is an optimization to expand a call to a method in the
            // same ClassExp.  The result is a call to a PrimProcedure instead.
            // This isn't just an optimization, since the re-write is
            // needed to ensure that if we're in an inner lambda that the
            // $this$ declaration is captured in a closure.
	    PrimProcedure mproc = new PrimProcedure(method, lambda);
            Expression[] margs;
            if (mproc.getStaticFlag())
              margs = exp.args;
            else
              {
                LambdaExp curLambda = getCurrentLambda();
                for (;;)
                  {
                    if (curLambda == null)
                      return noteError("internal error: missing "+lambda);
                    if (curLambda.outer == lambda.outer) // I.e. same class.
                      break;
                    curLambda = curLambda.outerLambda();
                  }
                Declaration d = curLambda.firstDecl();
                if (d==null || ! d.isThisParameter())
                  return noteError("calling non-static method "
                                   +lambda.getName()+" from static method "
                                   +curLambda.getName());
                margs = new Expression[1 + nargs];
                System.arraycopy(exp.getArgs(), 0, margs, 1, nargs);
                margs[0] = new ThisExp(d);
              }
            ApplyExp nexp = new ApplyExp(mproc, margs);
            return nexp.setLine(exp);
          }
      }
    return exp;
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

  protected Expression walkClassExp (ClassExp exp)
  {
    // Give name to object class.
    exp.setParts(this, comp);
    comp.addClass(exp.type);
    if (exp.isMakingClassPair())
      {
	exp.instanceType.setName(exp.type.getName()+"$class");
	comp.addClass(exp.instanceType);
      }
    return super.walkClassExp(exp);
  }
}
