// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.Type;

/** Similar to a CLOS method.
 * Can check if arguments "match" before committing to calling method. */

public abstract class MethodProc extends ProcedureN
{
  /** Return a buffer that can contain decoded (matched) arguments. */
  public Object getVarBuffer()
  {
    return new Object[maxArgs()];
  }

  /** Test if method is applicable to an invocation with given arguments.
   * Returns -1 if no; 1 if yes; 0 if need to check at run-time. */
  public int isApplicable(Type[] argTypes)
  {
    int argCount = argTypes.length;
    int num = numArgs();
    if (argCount < (num & 0xFFF)
	|| (num >= 0 && argCount > (num >> 12)))
      return -1;
    int result = 1;
    for (int i = argCount;  --i >= 0; )
      {
        Type ptype = getParameterType(i);
        int code = ptype.compare(argTypes[i]);
        if (code == -3)
          return -1;
        if (code < 0)
          result = 0;
      }
    return result;
  }

  /** Return number of parameters, including optional and rest arguments. */
  public int numParameters()
  {
    int num = numArgs();
    int max = num >> 12;
    if (max >= 0)
      return max;
    // This isn't really right, but it works for PrimProcedure.  FIXME.
    int min = num & 0xFFF;
    return min + 1;
  }

  public Type getParameterType(int index)
  {
    return Type.pointer_type;
  }

  /** Match the incoming arguments.
   * @param args the incoming argument list
   * @param vars where to save the matched result on success
   * @return null if the match succeeded, else an exception
   */
  public abstract RuntimeException match (Object vars, Object[] args);

  public Object match (Object[] args)
  {
    Object vars = getVarBuffer();
    return match(vars, args) == null ? vars : null;
  }

  public abstract Object applyV(Object vars);

  public Object applyN(Object[] args)
  {
    checkArgCount(this, args.length);
    Object vars = getVarBuffer();
    RuntimeException err = match(vars, args);
    if (err != null)
      throw err;
    return applyV(vars);
  }

  /** Return the more specific of the arguments.
   * @return null if neither is more specific. */
  public static MethodProc mostSpecific(MethodProc proc1, MethodProc proc2)
  {
    // True if we've determined proc1 cannot be the more specific.
    boolean not1 = false;
    // True if we've determined proc2 cannot be the more specific.
    boolean not2 = false;
    int min1 = proc1.minArgs();
    int min2 = proc2.minArgs();
    int max1 = proc1.maxArgs();
    int max2 = proc2.maxArgs();
    int num1 = proc1.numParameters();
    int num2 = proc2.numParameters();
    int limit = num1 > num2 ? num1 : num2;
    if (max1 != max2)
      {
        if (max1 < 0)
          not2 = true;
        if (max2 < 0)
          not1 = true;
      }
    if (min1 < min2)
      not2 = true;
    else if (min1 > min2)
      not1 = true;
    for (int i = 0; i < limit; i++)
      {
        Type t1 = proc1.getParameterType(i);
        Type t2 = proc2.getParameterType(i);
	int comp = t1.compare(t2);
        if (comp == -1)
          {
            not2 = true;
            if (not1)
              return null;
          }
        if (comp == 1)
          {
            not1 = true;
            if (not2)
              return null;
          }
      }
    return not2 ? proc1 : not1 ? proc2 : null;
  }

  /** Return the index of the most specific method. */
  public static int mostSpecific(MethodProc[] procs, int length)
  {
    MethodProc best = null;
    if (length == 0)
      return -1;
    int result = 0;
    best = procs[0];
    for (int i = 1;  i < length;  i++)
      {
        MethodProc method = procs[i];
        best = mostSpecific(best, method);
        if (best == null)
          return -1;
        else if (best == method)
          result = i;
      }
    return result;
  }
}
