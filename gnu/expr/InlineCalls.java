package gnu.expr;
import gnu.mapping.*;
import gnu.text.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.Invoke;

public class InlineCalls extends ExpWalker
{
  Compilation comp;

  /** Get the Compilation associated with this walker. */
  public Compilation getCompilation () { return comp; }

  public static void inlineCalls (Expression exp, Compilation comp)
  {
    InlineCalls walker = new InlineCalls();
    walker.comp = comp;
    walker.messages = comp.getMessages();
    walker.walk(exp);
  }

  /** Possibly convert a Symbol method call to invokeStatic or make. */
  Expression rewriteToInvocation(Symbol sym, ApplyExp aexp)
  {
    Expression[] args = aexp.args;
    String uri = sym.getNamespaceURI();
    if (uri == null || ! uri.startsWith("class:"))
      return null;
    String className = uri.substring(6);
    String methodName = sym.getName();
    ClassType typeInvoke = ClassType.make("gnu.kawa.reflect.Invoke");
    String invFieldName;
    Invoke invProc;
    boolean isNew = methodName.equals("new");
    if (isNew)
      {
	invFieldName = "make";
	invProc = gnu.kawa.reflect.Invoke.make;
      }
    else
      {
	invFieldName = "invokeStatic";
	invProc = gnu.kawa.reflect.Invoke.invokeStatic;
      }
    Field invField = typeInvoke.getDeclaredField(invFieldName);
    Declaration invDecl = new Declaration("invoke", invField);
    invDecl.noteValue(new QuoteExp(invProc));
    invDecl.setFlag(Declaration.IS_CONSTANT);
    Expression[] xargs = new Expression[args.length + (isNew ? 1 : 2)];
    System.arraycopy(args, 0, xargs, (isNew ? 1 : 2), args.length);
    xargs[0] = new QuoteExp(className);
    if (! isNew)
      xargs[1] = new QuoteExp(methodName);
    args = xargs;
    ApplyExp nexp = new ApplyExp(new ReferenceExp(invDecl), args);
    nexp.setLine(aexp);
    return invProc.inline(nexp, this);
  }

  protected Expression walkApplyExp(ApplyExp exp)
  {
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
            func = decl.getValue();
	    if (func instanceof LambdaExp) 
	      lambda = (LambdaExp) func;
	  }
	else if (rexp.getSymbol() instanceof Symbol)
	  {
	    Expression inv
	      = rewriteToInvocation((Symbol) rexp.getSymbol(), exp);
	    if (inv != null)
	      return inv;
	  }
		 
      }
    if (func instanceof QuoteExp)
      {
	Object fval = ((QuoteExp) func).getValue();
	if (! (fval instanceof Procedure))
	  return noteError(decl == null ? "called value is not a procedure"
			   : ("calling " + decl.getName()
			      + " which is not a procedure"));
	Procedure proc = (Procedure) fval;
	String msg = WrongArguments.checkArgCount(proc, nargs);
	if (msg != null)
	  return noteError(msg);
	if (proc instanceof CanInline)
	  return ((CanInline) proc).inline(exp, this);
	if (comp.inlineOk(proc))
	  {
	    PrimProcedure mproc
	      = PrimProcedure.getMethodFor(proc, decl, exp.args,
					   comp.getInterpreter());
	    if (mproc != null)
	      {
		ApplyExp nexp;
		if (mproc.getStaticFlag())
		  nexp = new ApplyExp(mproc, exp.args);
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
      }
    return exp;
  }

  protected Expression walkIfExp (IfExp exp)
  {
    exp.walkChildren(this);
    Expression test = exp.test;
    if (test instanceof QuoteExp)
      {
	Interpreter interpreter = comp.getInterpreter();
	if (interpreter.isTrue(((QuoteExp) test).getValue()))
	  return exp.then_clause;
	else
	  return exp.else_clause;
      }
    return exp;
  }

  protected Expression walkSetExp (SetExp exp)
  {
    Declaration decl = exp.binding;
    boolean updateNeeded = false;
    if (decl != null)
      {
        Expression declValue = decl.getValue();
        if (declValue == exp.new_value)
          updateNeeded = true;
      }
    exp.walkChildren(this);
    if (updateNeeded)
      {
        decl.value = exp.new_value;
        if (exp.new_value instanceof LambdaExp)
          ((LambdaExp) exp.new_value).nameDecl = decl;
      }
    return exp;
  }

  protected Expression walkClassExp (ClassExp exp)
  {
    // Give name to object class.
    exp.getCompiledClassType(comp);
    comp.addClass(exp.type);
    if (exp.isMakingClassPair())
      {
	exp.instanceType.setName(exp.type.getName()+"$class");
	comp.addClass(exp.instanceType);
      }
    return super.walkClassExp(exp);
  }
}
