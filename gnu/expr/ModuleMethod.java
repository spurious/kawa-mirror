// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;

/** Call a specified method in in a ModuleBody.
 * We use an extra level of indirection, but we save by having
 * to create fewer classes than in the one-class-per-procedure
 * scheme, without having to use (slow) reflection. */

public class ModuleMethod extends ProcedureN
// future:  extends MethodProc
{
  ModuleBody module;
  public final int selector;
  private int numArgs;

  public ModuleMethod(ModuleBody module, int selector,
                      String name, int numArgs)
  {
    this.module = module;
    this.selector = selector;
    this.numArgs = numArgs;
    setName(name);
  }

  public int numArgs() { return numArgs; }

  public Object apply0()
  {
    return module.apply0(this);
  }

  public Object apply1(Object arg1)
  {
    return module.apply1(this, arg1);
  }

  public Object apply2(Object arg1, Object arg2)
  {
    return module.apply2(this, arg1, arg2);
  }

  public Object apply3(Object arg1, Object arg2, Object arg3)
  {
    return module.apply3(this, arg1, arg2, arg3);
  }

  public Object applyN(Object[] args)
  {
    return module.applyN(this, args);
  }
}
