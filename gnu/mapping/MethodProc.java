// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.bytecode.Type;

/** Similar to a CLOS method.
 * Can check if arguments "match" before committing to calling method. */

public abstract class MethodProc extends ProcedureN
{
  /** The parameter types.
   * Usually either an Type[] or a String encoding. */
  protected Object argTypes;

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

  static final Type[] unknownArgTypes = { Type.pointer_type };

  /** Figure out or decode the parameter types, setting argTypes. */
  protected void resolveParameterTypes()
  {
    argTypes = unknownArgTypes;
  }

  public Type getParameterType(int index)
  {
    if (! (argTypes instanceof Type[]))
      resolveParameterTypes();

    Type[] atypes = (Type[]) argTypes;
    if (index >= atypes.length)
      index = atypes.length - 1;
    return atypes[index];
  }

  /** Match the incoming arguments.
   * @param ctx where to save the matched result on success
   * @param args the incoming argument list
   * @return non-negative if the match succeeded, else negative
   */
  public abstract int match (CallContext ctx, Object[] args);
  // FUTURE:
  // On success, vars has been initialized so vars.run() will work.
  // public abstract int match(CallContext vars);

  /** Return code from match:  Unspecified failure. */
  public static final int NO_MATCH = -1;

  /** Return code from match:  Too few actual arguments.
   * The lower half is the minimum number of arguments (if not 0xffff). */
  public static final int NO_MATCH_TOO_FEW_ARGS = 0xfff10000;

  /** Return code from match:  Too many actual arguments.
   * The lower half is the maximum number of arguments (if not 0xffff). */
  public static final int NO_MATCH_TOO_MANY_ARGS = 0xfff20000;

  /** Return code from match:  Ambigious which method to select. */
  public static final int NO_MATCH_AMBIGUOUS = 0xfff30000;

  /** Return code from match: Invalid argument type.
   * In that case the lower half is the 0-origin index of the first
   * argument that does not match. */
  public static final int NO_MATCH_BAD_TYPE = 0xfff40000;

  public Object match (Object[] args)
  {
    CallContext ctx = new CallContext();
    return match(ctx, args) == 0 ? ctx : null;
    // FUTURE:
    // vars.setArgs(args);
    // return match(vars) == 0 ? vars : null;
  }

  public abstract Object applyV(CallContext ctx) throws Throwable;
  // FUTURE:
  // ctx.run();

  public static RuntimeException
  matchFailAsException(int code, Procedure proc, Object[] args)
  {
    int arg = (short) code;
    code &= 0xffff0000;
    if (code == NO_MATCH_TOO_FEW_ARGS || code == NO_MATCH_TOO_MANY_ARGS)
      return new WrongArguments(proc, args.length);
    if (code != NO_MATCH_BAD_TYPE)
      arg = WrongType.ARG_UNKNOWN;
    throw new WrongType(proc, arg, null);
  }

  public Object applyN(Object[] args) throws Throwable
  {
    checkArgCount(this, args.length);
    CallContext vars = new CallContext();
    int err = match(vars, args);
    if (err != 0)
      throw matchFailAsException(err, this, args);
    // FUTURE:
    // vars.run();
    // return vars.getResult();
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
