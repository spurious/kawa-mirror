// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import java.lang.reflect.*;

/** Call a specified method in in a ModuleBody.
 * We use an extra level of indirection, but we save by having
 * to create fewer classes than in the one-class-per-procedure
 * scheme, without having to use (slow) reflection.
 *
 * ModuleMethod is redundant, since it could be replaced by ApplyMethodProc.
 * However, ModuleMethod uses virtual method calls, while ApplyMethodProc
 * uses the possibly much slower interface method calls.
 */

public class ModuleMethod extends MethodProc
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

  /** Figure out parameter types.
   * Uses reflection to get method parameter types.
   * INCOMPLETE - does not handle procedures with optional or rest args. */
  protected void resolveParameterTypes()
  {
    Method method = null;
    try
      {
	Class moduleClass = module.getClass();
	Method[] methods = moduleClass.getDeclaredMethods();
	String mangledName = Compilation.mangleNameIfNeeded(getName());
	for (int i = methods.length;  --i >= 0; )
	  {
	    if (methods[i].getName().equals(mangledName))
	      {
		if (method != null)
		  {
		    method = null;
		    break;
		  }
		method = methods[i];
	      }
	  }
	if (method != null)
	  {
	    Interpreter interp = Interpreter.getInterpreter();
	    Class[] parameterClasses = method.getParameterTypes();
	    int numParamTypes = parameterClasses.length;
	    gnu.bytecode.Type[] atypes = new gnu.bytecode.Type[numParamTypes];
	    for (int i = numParamTypes;  --i >= 0; )
	      {
		atypes[i] = interp.getTypeFor(parameterClasses[i]);
	      }
	    this.argTypes = atypes;
	  }
      }
    catch (Throwable ex)
      {
      }
    if (argTypes == null)
      super.resolveParameterTypes();
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

  public int match (CallContext ctx, Object[] args)
  {
    int argCount = args.length;
    int num = numArgs();
    int min = num & 0xFFF;
    if (argCount < min)
      return NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
        if (argCount > max)
          return NO_MATCH_TOO_MANY_ARGS|max;
      }
    for (int i = 0;  i < args.length;  i++)
      {
	Object argi = args[i];
	if (argi != null && ! getParameterType(i).isInstance(argi))
	  return NO_MATCH_BAD_TYPE|i;
      }
    ctx.value1 = args;
    return 0;
  }

  public Object applyV(CallContext ctx)
  {
    return applyN((Object[]) ctx.value1);
  }
}
