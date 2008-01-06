package gnu.xquery.util;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

/** A procedure used to represent a FLWOR expression with
 * an {@code order by} clause.
 * ({@link gnu.kawa.functions.ValuesMap} is used for FLWOR expression
 * that don't have an {@code order by} clause.)
 *
 * As returned by the parser:
 * <pre>
 * for $x1 in exp1, $x2 in exp2 where cond order by comparator1 ... return body
 * </pre>
 * is represented as
 * <pre>
 * ordered-map(tuple-sequence, body-function,
 *             comparator-function1, flags1, collation1, ...)
 * </pre>
 * Here tuple-sequence is an expression that returns a sequence of tuples,
 * which are currently implemnted as Java Object[] arrays.
 * After inlining we get:
 * <pre>
 * ordered-map(tuple-sequence.
 *   OrderedTuples.make$V(body-function,
 *     new Object[]{comparator-function1, flags1, collation1, ...}))
 * </pre>
 *
 * A future optimization would be to create an instance of a new sub-class
 * of OrderedTuples.  Then the body-function and comparator-functions
 * could be compiled as methods to that class.  That wins especially
 * if it saves us having to create extra frame classes.
 */

public class OrderedMap extends MethodProc
  implements CanInline, Inlineable
{
  public static final OrderedMap orderedMap = new OrderedMap();

  public static Object[] makeTuple$V (Object[] values)
  {
    return values;
  }

  static final ClassType typeTuples
    = ClassType.make("gnu.xquery.util.OrderedTuples");

  public Expression inline (ApplyExp exp, InlineCalls walker)
  {
    exp.walkArgs(walker);
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      {
        Expression[] rargs = new Expression[args.length-1];
        System.arraycopy(args, 1, rargs, 0, rargs.length);
        Expression[] xargs = new Expression[2];
        Method makeTupleMethod = typeTuples.getDeclaredMethod("make$V", 2); 
        xargs[0] = args[0];
        xargs[1] = new ApplyExp(makeTupleMethod, rargs);
        return new ApplyExp(this, xargs);
      }
    return exp;
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    Object values = args[0];
    OrderedTuples tuples;
    if (args.length == 2)
      {
        tuples = (OrderedTuples) args[1];
      }
    else
      {
        Object[] comps = new Object[args.length-2];
        System.arraycopy(args, 2, comps, 0, comps.length);
        tuples = OrderedTuples.make$V((Procedure) args[1], comps);
      }
    Values.writeValues(values, tuples);
    tuples.run$X(ctx);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      {
        ApplyExp.compile(exp, comp, target);
        return;
      }
    CodeAttr code = comp.getCode();
    Scope scope = code.pushScope();
    Variable consumer = scope.addVariable(code, typeTuples, null);
    args[1].compile(comp, Target.pushValue(typeTuples));
    code.emitStore(consumer);
    ConsumerTarget ctarget = new ConsumerTarget(consumer);
    args[0].compile(comp, ctarget);
    Method mm = typeTuples.getDeclaredMethod("run$X", 1);
    code.emitLoad(consumer);
    PrimProcedure.compileInvoke(comp, mm, target, exp.isTailCall(),
                                182/*invokevirtual*/, Type.pointer_type);
    code.popScope();
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return Type.pointer_type; // FIXME
  }
}
