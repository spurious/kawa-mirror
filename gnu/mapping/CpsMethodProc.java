// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class CpsMethodProc extends CpsProcedure
{
  CpsMethodContainer module;
  public final int selector;
  private int numArgs;

  public String getName ()
  {
    String name = super.getName();
    if (name == null)
      {
	name = "{" + module.getClass().getName() + ':' + selector + '}';
      }
    return name;
  }

  public CpsMethodProc(CpsMethodContainer module, int selector,
			String name, int numArgs)
  {
    this.module = module;
    this.selector = selector;
    this.numArgs = numArgs;
    setName(name);
  }

  public CpsMethodProc(CpsMethodContainer module, int selector,
			String name, int numArgs, Object argTypes)
  {
    this.module = module;
    this.selector = selector;
    this.numArgs = numArgs;
    setName(name);
    this.argTypes = argTypes;
  }

  /*
  protected void resolveParameterTypes()
  {
  }
  */

  public int numArgs() { return numArgs; }

  /*
  public final int match (CallContext ctx, Object[] args)
  {
    ctx.setArgsN(args);
    return modele.match(this, ctx);
  }
  */

  public void apply (CallContext context)
  {
    module.apply(this, context);
    /*
    int code = module.match(this, context);
    if (code == 0)
      module.apply(this, context);
    else
      {
	int arg = (short) code;
	code &= 0xffff0000;
	if (code == NO_MATCH_TOO_FEW_ARGS || code == NO_MATCH_TOO_MANY_ARGS)
	  throw new WrongArguments(this, context.count);
	if (code != NO_MATCH_BAD_TYPE)
	  arg = WrongType.ARG_UNKNOWN;
	throw new WrongType(this, arg, null);
      }
    */
  }

  /** Helper methods for default CpsMethodContainer actions. */

  public void applyError()
  {
    throw new Error("internal error - bad selector for "+this);
  }
  
}
