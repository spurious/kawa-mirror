package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/** Evaluates to the "this" implicit variable.
 * This is currently neither robust nor general.  FIXME!
 */

public class ThisExp extends ReferenceExp
{
  /** When evaluating, return the context.
   * This is used for the "context" of a Macro.
   */
  static int EVAL_TO_CONTEXT = NEXT_AVAIL_FLAG;

  /** The class which this refers to. */
  ScopeExp context;

  public Object eval (Environment env)
  {
    if ((flags & EVAL_TO_CONTEXT) != 0)
      return context;
  return super.eval(env);
  }

  public ScopeExp getContextScope () { return context; }

  public ThisExp ()
  {
    super("$this$");
  }

  public ThisExp(ScopeExp context)
  {
    super("$this$");
    this.context = context;
  }

  public ThisExp (Declaration binding)
  {
    super("$this", binding);
  }

  public ThisExp (ClassType type)
  {
    this(new Declaration("this", type));
  }

  public static ThisExp makeGivingContext (ScopeExp context)
  {
    ThisExp exp = new ThisExp(context);
    exp.flags |= EVAL_TO_CONTEXT;
    return exp;
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    CodeAttr code = comp.getCode();
    if (comp.method.getStaticFlag())
      code.emitGetStatic(comp.moduleInstanceMainField);
    else
      code.emitPushThis();
    target.compileFromStack(comp, getType());
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkThisExp(this);
  }

  public final gnu.bytecode.Type getType()
  {
    if (binding != null)
      return binding.getType();
    if (context instanceof ClassExp || context instanceof ModuleExp)
      return context.getType();
    return Type.pointer_type;
  }
}
