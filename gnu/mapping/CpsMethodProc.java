// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class CpsMethodProc extends CpsProcedure
{
  CpsMethodContainer module;
  public final int selector;
  private int numArgs;

  public CpsMethodProc(CpsMethodContainer module, int selector,
			String name, int numArgs)
  {
    this.module = module;
    this.selector = selector;
    this.numArgs = numArgs;
    setName(name);
  }

  public int numArgs() { return numArgs; }

  public void apply (CallContext context)
  {
    module.apply(this, context);
  }

  /** Helper methods for default CpsMethodContainer actions. */

  public void applyError()
  {
    throw new Error("internal error - bad selector for "+this);
  }
  
}
