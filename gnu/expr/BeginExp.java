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

  public BeginExp (Expression[] ex) { exps = ex; }

  public Object eval (Environment env)
  {
    int n = exps.length;
    int i;
    for (i = 0; i < n - 1; i++)
      exps[i].eval (env);
    return exps[i].eval (env);
  }

  public void compile (Compilation comp, Target target)
  {
    int n = exps.length, i;
    for (i = 0; i < n - 1; i++)
      exps[i].compileWithPosition(comp, Target.Ignore);
    exps[i].compileWithPosition(comp, target);
  }

  Object walk (ExpWalker walker) { return walker.walkBeginExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%begin");
    int n = exps.length;
    for (int i = 0; i < n; i++)
      { 
	ps.print('\n');
	exps[i].print (ps);
      }
    ps.print(")");
  }
}
