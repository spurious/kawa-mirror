package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

/**
 * Implement the Scheme standard function "/".
 * @author Per Bothner
 */

public class DivideOp extends ProcedureN implements CanInline
{
  /** True if result should be cast to integer.
   * Basically a hack (that should be generalized) for XQuery's
   * idiv operator. */
  boolean asInteger;

  public static final DivideOp $Sl = new DivideOp("/");
  public static final DivideOp idiv = new DivideOp("idiv");
  static { idiv.asInteger = true; }

  public DivideOp(String name)
  {
    super(name);
  }

  public Object applyN (Object[] args)
  {
    Numeric result;
    int i = 0;
    if (args.length == 1)
      result = IntNum.one ();
    else
      result = (Numeric) (args[i++]);
    for (; i < args.length;  i++)
      result = result.div (args[i]);
    if (asInteger)
      result = ((RealNum) result).toExactInt(Numeric.TRUNCATE);
    return result;
   }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Expression folded = exp.inlineIfConstant(this, walker);
    if (folded != exp)
      return folded;
    if (asInteger)
      return exp;
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return CompileArith.pairwise(this, exp.getFunction(), args, walker);
    if (args.length == 2)
      {
	Type type0 = args[0].getType();
	Type type1 = args[1].getType();
	int kind0 = AddOp.classify(type0);
	int kind1 = AddOp.classify(type1);
	if ((kind0 == 4 || type0.isSubtype(typeRatNum))
	    && (kind1 == 4 || type1.isSubtype(typeRatNum)))
	  return new ApplyExp(typeRatNum.getDeclaredMethod("divide", 2),
			      args);
	if (kind0 >= 3 && kind1 >= 3)
	  {
	    Expression opt = CompileArith.primInline(108, exp);
	    if (opt != exp)
	      return opt;
	  }
	if (kind0 >= 2 &&  kind1 >= 2)
	  return new ApplyExp(Arithmetic.typeRealNum.getDeclaredMethod("divide", 2),
			      args);
      }
    return exp;
  }

  static ClassType typeRatNum = ClassType.make("gnu.math.RatNum");
}
