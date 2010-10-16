// Copyright (c) 1999, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.bytecode.Type;
import gnu.bytecode.ArrayType;

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
    if (index < atypes.length
        && (index < atypes.length - 1 || maxArgs() >= 0))
      return atypes[index];
    if (maxArgs() < 0)
      {
        Type rtype = atypes[atypes.length-1];
        if (rtype instanceof ArrayType)
          return ((ArrayType) rtype).getComponentType();
      }
    return Type.objectType;
  }

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
   * In that case the lower half is the 1-origin index of the first
   * argument that does not match. */
  public static final int NO_MATCH_BAD_TYPE = 0xfff40000;

  /** Helper method to throw an exception if a <code>matchX</code>
   * method fails. */
  public static RuntimeException
  matchFailAsException(int code, Procedure proc, Object[] args)
  {
    int arg = (short) code;
    code &= 0xffff0000;
    if (code != NO_MATCH_BAD_TYPE)
      return new WrongArguments(proc, args.length);
    return new WrongType(proc, arg, arg > 0 ? args[arg-1] : null);
  }

  public Object applyN(Object[] args) throws Throwable
  {
    checkArgCount(this, args.length);
    CallContext ctx = CallContext.getInstance();
    checkN(args, ctx);
    return ctx.runUntilValue();
  }

  /** Return the more specific of the arguments.
   * @return null if neither is more specific. */
  public static MethodProc mostSpecific(MethodProc proc1, MethodProc proc2)
  {
    // True if we've determined proc1 cannot be the more specific.  I.e. there
    // can be aguments lists that are applicable to proc1 and not proc2.
    boolean not1 = false;
    // True if we've determined proc2 cannot be the more specific.
    boolean not2 = false;
    int min1 = proc1.minArgs();
    int min2 = proc2.minArgs();
    int max1 = proc1.maxArgs();
    int max2 = proc2.maxArgs();
    if ((max1 >= 0 && max1 < min2)
	|| (max2 >= 0 && max2 < min1))
      return null;
    int num1 = proc1.numParameters();
    int num2 = proc2.numParameters();
    int limit = num1 > num2 ? num1 : num2;
    if (max1 != max2)
      {
        if (max1 < 0)
          not1 = true;
        if (max2 < 0)
          not2 = true;
      }
    if (min1 < min2)
      not1 = true;
    else if (min1 > min2)
      not2 = true;
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
    if (length <= 1) // Handles length==0 and length==1.
      return length - 1;
    // best is non-null if there is a single most specific method.
    MethodProc best = procs[0];
    // This array (which is allocated lazily) is used if there is is a set
    // of bestn methods none of which are more specific than the others.
    MethodProc[] bests = null;
    // If best==null, then the index of the most specific method;
    // otherwise the active length of the bests array.
    int bestn = 0;
  outer:
    for (int i = 1;  i < length;  i++)
      {
        MethodProc method = procs[i];
	if (best != null)
	  {
	    MethodProc winner = mostSpecific(best, method);
	    if (winner == null)
	      {
		if (bests == null)
		  bests = new MethodProc[length];
		bests[0] = best;
		bests[1] = method;
		bestn = 2;
		best = null;
	      }
	    else if (winner == method)
	      {
		best = method;
		bestn = i;
	      }
	  }
	else
	  {
	    for (int j = 0;  j < bestn;  j++)
	      {
		MethodProc old = bests[j];
		MethodProc winner = mostSpecific(old, method);
		if (winner == old)
		  continue outer;
		if (winner == null)
		  {
		    bests[bestn++] = method;
		    continue outer;
		  }
	      }
	    // At this point method is more specific than bests[0..bestn-1].
	    best = method;
	    bestn = i;
	  }
      }
    return best == null ? -1 : bestn;
  }
}
