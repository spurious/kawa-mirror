// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/**
 * Class used to implement a block that can be exited.
 * @author	Per Bothner
 */

public class BlockExp extends Expression
{
  Declaration label;
  Expression body;

  /* Target used to evaluate body. Temporary only used during compilation. */
  Target subTarget;
  /* Label to exit to.  Temporary only used during compilation. */
  Label exitLabel;

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget
	|| target == Target.pushObject)
      subTarget = target;
    else
      {
	// We can probably do better - and this is probably
	// wrong for TailTargets.  FIXME.
	subTarget = new StackTarget(getType());
      }
    gnu.bytecode.CodeAttr code = comp.getCode();
    Label exitLabel = new Label(code);
    body.compileWithPosition(comp, subTarget);
    exitLabel.define(code);
    if (subTarget != target)
      target.compileFromStack(comp, subTarget.getType());
  }

  Object walk (ExpWalker walker) { return walker.walkBlockExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%block ");
    ps.print(label.getName());
    ps.print(' ');
    body.print(ps);
    ps.print(')');
  }
}
