package gnu.expr;
import gnu.mapping.*;

/**
 * This class represents a sequence of Expressions.
 * The expressions are evaluated for their side-effects,
 * and the value of the last Expression is the result.
 * @author	Per Bothner
 */

public class BeginExp extends Expression
{
  Expression[] exps;
  int length;

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
        int len = bexp.length;
        if (len == 0)
          return QuoteExp.voidExp;
        if (len == 1)
          return canonicalize(bexp.exps[0]);
      }
    return exp;
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

  public Object eval (Environment env) throws Throwable
  {
    int n = length;
    int i;
    for (i = 0; i < n - 1; i++)
      exps[i].eval (env);
    return exps[i].eval (env);
  }

  public void compile (Compilation comp, Target target)
  {
    int n = length, i;
    for (i = 0; i < n - 1; i++)
      exps[i].compileWithPosition(comp, Target.Ignore);
    exps[i].compileWithPosition(comp, target);
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkBeginExp(this);
  }

  protected void walkChildren(ExpWalker walker)
  {
    exps = walker.walkExps(exps, length);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Begin", ")", 2);
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
