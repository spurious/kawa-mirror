// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** Call a specified method in in a ApplyMethodContainer.
 * We use an extra level of indirection, but we save by having
 * to create fewer classes than in the one-class-per-procedure
 * scheme, without having to use (slow) reflection.
 *
 * ApplyMethodProc is a generalization of expr.ModuleMethod.
 */

public class ApplyMethodProc extends ProcedureN
{
  ApplyMethodContainer module;
  public final int selector;
  private int numArgs;

  public ApplyMethodProc(ApplyMethodContainer module, int selector,
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

  public Object apply4(Object arg1, Object arg2, Object arg3, Object arg4)
  {
    return module.apply4(this, arg1, arg2, arg3, arg4);
  }

  public Object applyN(Object[] args)
  {
    return module.applyN(this, args);
  }

  /** Helper methods for default ApplyMethodContainer actions. */

  public static Object apply0Default(ApplyMethodProc method)
  {
    return method.module.applyN(method, Values.noArgs);
  }

  public static Object apply1Default(ApplyMethodProc method, Object arg1)
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return method.module.applyN(method, args);
  }

  public static Object apply2Default(ApplyMethodProc method, Object arg1, Object arg2)
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return method.module.applyN(method, args);
  }

  public static Object apply3Default(ApplyMethodProc method,
                       Object arg1, Object arg2, Object arg3)
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return method.module.applyN(method, args);
  }

  public static Object apply4Default(ApplyMethodProc method,
                       Object arg1, Object arg2, Object arg3, Object arg4)
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return method.module.applyN(method, args);
  }

  public static Object applyNDefault(ApplyMethodProc method, Object[] args)
  {
    int count = args.length;
    int num = method.numArgs();
    ApplyMethodContainer module = method.module;
    if (count >= (num & 0xFFF)
	&& (num < 0 || count <= (num >> 12)))
      {
        switch (count)
          {
          case 0:
            return module.apply0(method);
          case 1:
            return module.apply1(method, args[0]);
          case 2:
            return module.apply2(method, args[0], args[1]);
          case 3:
            return module.apply3(method, args[0], args[1], args[2]);
          case 4:
            return module.apply4(method, args[0], args[1], args[2], args[3]);
          }
      }
    throw new WrongArguments(method, count);
  }
}
