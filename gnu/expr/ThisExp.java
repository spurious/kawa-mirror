package gnu.expr;
import gnu.bytecode.*;

/** Evaluates to the "this" implicit variable.
 * This is currently neither robust nor general.  FIXME!
 */

public class ThisExp extends ReferenceExp
{
  public ThisExp ()
  {
    super("$this$");
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    code.emitPushThis();
  }

  Object walk (ExpWalker walker) { return walker.walkThisExp(this); }
}
