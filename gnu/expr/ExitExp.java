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

  public ExitExp(Expression result, BlockExp block)
  {
    this.result = result;
    this.block = block;
  }

  public ExitExp(BlockExp block)
  {
    this.result = QuoteExp.voidExp;
    this.block = block;
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    Expression res = result == null ? QuoteExp.voidExp : result;
    res.compileWithPosition(comp, block.subTarget);
    code.emitGoto(block.exitLabel);
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkExitExp(this);
  }

  protected void walkChildren (ExpWalker walker)
  {
    result = result.walk(walker);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%exit ");
    if (block == null || block.label == null)
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

  public Type getType()
  {
    return Type.neverReturnsType;
  }
}
