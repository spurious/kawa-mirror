package gnu.kawa.functions;
import gnu.math.IntNum;
import gnu.math.Numeric;
import gnu.expr.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "*".
 * @author Per Bothner
 */

public class MultiplyOp extends ProcedureN implements CanInline
{
  public static final MultiplyOp $St = new MultiplyOp("*");

  public MultiplyOp(String name)
  {
    super(name);
  }

  public static Object apply (Object arg1, Object arg2)
  {
    return ((Numeric) arg1).mul(arg2);
  }

  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.one ();
    Numeric result = (Numeric) args[0];
    for (int i = 1; i < len; i++)
      result = result.mul (args[i]);
    return result;
   }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression folded = exp.inlineIfConstant(this, walker);
    if (folded != exp)
      return folded;
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return AddOp.pairwise(this, exp.getFunction(), args, walker);
    if (args.length == 2)
      return AddOp.primInline(104, exp);
		
    return exp;
  }
}
