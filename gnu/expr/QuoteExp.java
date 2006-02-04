package gnu.expr;
import gnu.mapping.*;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  public final Object getValue() { return value; }

  public final gnu.bytecode.Type getType()
  {
    if (value == Values.empty)
      return gnu.bytecode.Type.void_type;
    if (value == null)
      return gnu.bytecode.Type.nullType;
    if (this == undefined_exp)
      return gnu.bytecode.Type.pointer_type;
    return gnu.bytecode.Type.make(value.getClass());
  }

  static public QuoteExp undefined_exp
  = new QuoteExp (Undefined.getInstance());
  static public QuoteExp voidExp = new QuoteExp (Values.empty);
  static public QuoteExp trueExp = new QuoteExp(Boolean.TRUE);
  static public QuoteExp falseExp = new QuoteExp(Boolean.FALSE);
  static public QuoteExp nullExp = new QuoteExp(null);

  public static QuoteExp getInstance (Object value)
  {
    if (value == null)
      return nullExp;
    if (value == Undefined.getInstance())
      return undefined_exp;
    if (value == Values.empty)
      return voidExp;
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue() ? trueExp : falseExp;
    return new QuoteExp(value);
  }

  public QuoteExp (Object val) { value = val; }
  
  public Object eval (Environment env)
  {
    return value;
  }

  public void compile (Compilation comp, Target target)
  {
    comp.compileConstant(value, target);
  }
 
  protected Expression walk (ExpWalker walker)
  {
    return walker.walkQuoteExp(this);
  }

  public Expression inline (ApplyExp exp, InlineCalls walker, Declaration decl)
  {
    if (this == QuoteExp.undefined_exp)
      return exp;
    Object fval = getValue();
    if (! (fval instanceof Procedure))
      return walker.noteError(decl == null || fval == null ? "called value is not a procedure"
			      : ("calling " + decl.getName()
				 + " which is a "+fval.getClass().getName()));
    Procedure proc = (Procedure) fval;
    int nargs = exp.getArgCount();
    String msg = WrongArguments.checkArgCount(proc, nargs);
    if (msg != null)
      return walker.noteError(msg);
    if (proc instanceof CanInline)
      return ((CanInline) proc).inline(exp, walker);
    if (exp.getFlag(ApplyExp.INLINE_IF_CONSTANT))
      {
	Expression e = exp.inlineIfConstant(proc, walker);
	if (e != exp)
	  return walker.walk(e);
      }
    Compilation comp = walker.getCompilation();
    if (comp.inlineOk(proc))
      {
	if (proc instanceof Inlineable)
	  return new ApplyExp(this, exp.getArgs()).setLine(exp);
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
    return exp;
  }

  public String toString ()
  {
    return "QuoteExp["+value+"]";
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Quote", ")", 2);
    out.writeSpaceLinear();
    if (value instanceof Expression)
      value = value.toString(); // To avoid cycles.
    gnu.lists.AbstractFormat saveFormat = out.objectFormat;
    try
      {
	out.objectFormat = Language.getDefaultLanguage().getFormat(true);
	out.print(value);
      }
    finally
      {
	out.objectFormat = saveFormat;
      }
    out.endLogicalBlock(")");
  }
}
