// Copyright (c) 1999, 2006, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

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

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    throw new BlockExitException(this, result.eval(ctx));
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    Expression res = result == null ? QuoteExp.voidExp : result;
    res.compileWithPosition(comp, block.subTarget);
    code.doPendingFinalizers(block.oldTryState);
    code.emitGoto(block.exitLabel);
  }

  protected Expression deepCopy (gnu.kawa.util.IdentityHashTable mapper)
  {
    Expression res = deepCopy(result, mapper);
    if (res == null && result != null)
      return null;
    Object b = mapper.get(block);
    ExitExp copy
      = new ExitExp((Expression) res, b == null ? block : (BlockExp) b);
    copy.flags = getFlags();
    return copy;
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkExitExp(this);
  }

  protected void walkChildren (ExpWalker walker)
  {
    result = walker.walk(result);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Exit", false, ")");
    out.writeSpaceFill();
    if (block == null || block.label == null)
      out.print("<unknown>");
    else
      out.print(block.label.getName());
    if (result != null)
      {
	out.writeSpaceLinear();
	result.print(out);
      }
    out.endLogicalBlock(")");
  }

  public Type getType()
  {
    return Type.neverReturnsType;
  }
}
