// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/**
 * Expression to exit a lexically surrounding block.
 * @author	Per Bothner
 */

public class ExitExp extends Expression
{
  BlockExp block;
  Expression result;

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    Expression res = result == null ? QuoteExp.voidExp : result;
    res.compileWithPosition(comp, block.subTarget);
    code.emitGoto(block.exitLabel);
    // Normally target is an IgnoreTarget, but just in case it isn't:
    QuoteExp.voidExp.compile(comp, target);
  }

  Object walk (ExpWalker walker) { return walker.walkExitExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%exit ");
    if (block == null)
      ps.print("<unknown>");
    else
      ps.print(block.label.getName());
    if (result != null)
      {
	ps.print(' ');
	result.print(ps);
      }
    ps.print(')');
  }

}
