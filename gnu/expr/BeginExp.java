package gnu.expr;
import gnu.mapping.*;
import java.util.Vector;

/**
 * This class represents a sequence of Expressions.
 * The expressions are evaluated for their side-effects,
 * and the value of the last Expression is the result.
 * A BeginExp may optionally have "compilation options"
 * which can be used to control various compile-time
 * aspects of Kawa, such as warning messages.
 * @author	Per Bothner
 */

public class BeginExp extends Expression
{
  Expression[] exps;
  int length;

  /** A Vector used to remember compile options.
   * This is used to set/reset the options in Compilations's currentOptions.
   * Each option consists of 3 elements of the Vector:
   * A key String that names the option;
   * a place to save the old value of the option;
   * the value the value the option should have during traversal
   * (using an ExpWalker or compilation) of this BeginExp.
   * Note traversal is not thread-safe because the "old value" is saved 
   * in this same array.  A cleaner (future) solution might be to use
   * a stack in the Compilation.  Since expressions (except for QuoteExp)
   * are local to a specific Compilation, it doesn't matter.
   */
  Vector compileOptions;

  public BeginExp () { }

  public BeginExp (Expression[] ex) { exps = ex;  length = ex.length; }

  public BeginExp(Expression exp0, Expression exp1)
  {
    exps = new Expression[2];
    exps[0] = exp0;
    exps[1] = exp1;
    length = 2;
  }

  /** Simplifies BeginExp.
   * (In the future, nested BeginExps may be "flattened" as well.)
   */
  public static final Expression canonicalize(Expression exp)
  {
    if (exp instanceof BeginExp)
      {
        BeginExp bexp = (BeginExp) exp;
	if (bexp.compileOptions != null)
	  return exp;
        int len = bexp.length;
        if (len == 0)
          return QuoteExp.voidExp;
        if (len == 1)
          return canonicalize(bexp.exps[0]);
      }
    return exp;
  }

  public static final Expression canonicalize(Expression[] exps)
  {
    int len = exps.length;
    if (len == 0)
      return QuoteExp.voidExp;
    if (len == 1)
      return canonicalize(exps[0]);
    return new BeginExp(exps);
  }

  public final void add(Expression exp)
  {
    if (exps == null)
      exps = new Expression[8];
    if (length == exps.length)
      {
        Expression[] ex = new Expression[2 * length];
        System.arraycopy(exps, 0, ex, 0, length);
        exps = ex;
      }
    exps[length++] = exp;
  }

  public final Expression[] getExpressions() { return exps; }

  public final void setExpressions(Expression[] exps)
  {
    this.exps = exps;
    length = exps.length;
  }

  public void setCompileOptions (Vector options)
  {
    compileOptions = options;
  }

  public Object eval (Environment env) throws Throwable
  {
    int n = length;
    int i;
    for (i = 0; i < n - 1; i++)
      exps[i].eval (env);
    return exps[i].eval (env);
  }

  public void pushOptions (Compilation comp)
  {
    if (compileOptions != null && comp != null)
      comp.currentOptions.pushOptionValues(compileOptions);
  }

  public void popOptions (Compilation comp)
  {
    if (compileOptions != null && comp != null)
      comp.currentOptions.popOptionValues(compileOptions);
  }

  public void compile (Compilation comp, Target target)
  {
    pushOptions(comp);
    try
      {
	int n = length, i;
	for (i = 0; i < n - 1; i++)
	  exps[i].compileWithPosition(comp, Target.Ignore);
	exps[i].compileWithPosition(comp, target);
      }
    finally
      {
	popOptions(comp);
      }
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkBeginExp(this);
  }

  protected void walkChildren(ExpWalker walker)
  {
    pushOptions(walker.comp);
    try
      {
	exps = walker.walkExps(exps, length);
      }
    finally
      {
	popOptions(walker.comp);
      }
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Begin", ")", 2);
    out.writeSpaceFill();
    printLineColumn(out);
    if (compileOptions != null)
      {
	out.writeSpaceFill();
	out.startLogicalBlock("(CompileOptions", ")", 2);
	int sizeOptions = compileOptions.size();
	for (int i = 0;  i < sizeOptions;  i += 3)
	  {
	    Object key = compileOptions.elementAt(i);
	    Object value = compileOptions.elementAt(i+2);
	    out.writeSpaceFill();
	    out.startLogicalBlock("", "", 2);
	    out.print(key);  out.print(':');
	    out.writeSpaceLinear();
	    out.print(value);
	    out.endLogicalBlock("");
	  }
	out.endLogicalBlock(")");
      }
    int n = length;
    for (int i = 0; i < n; i++)
      { 
	out.writeSpaceLinear();
	exps[i].print(out);
      }
    out.endLogicalBlock(")");
  }

  public gnu.bytecode.Type getType()
  {
    return exps[length - 1].getType();
  }
}
