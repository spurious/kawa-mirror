// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.Type;

/** A collection of MethodProcs;  one is chosen at apply time. */

public class GenericProc extends MethodProc
{
  MethodProc[] methods;
  int count;
  int minArgs;
  int maxArgs;

  public int numArgs()
  {
    return minArgs | (maxArgs << 12);
  }

  public void add(MethodProc method)
  {
    if (methods == null)
      methods = new MethodProc[8];
    else if (count >= methods.length)
      {
        MethodProc[] copy = new MethodProc[2 * methods.length];
        System.arraycopy(methods, 0, copy, 0, count);
        methods = copy;
      }

    int i;
    for (i = 0;  i < count;  i++)
      {
	MethodProc best = MethodProc.mostSpecific(method, methods[i]);
	if (best == method)
	  break;
      }
    if (i < count)
      System.arraycopy(methods, i, methods, i + 1, count - i);
    methods[i] = method;
    count++;

    int n = method.minArgs();
    if (n < minArgs)
      minArgs = n;
    n = method.maxArgs();
    if (n == -1 || n > maxArgs)
      maxArgs = n;
  }

  public Object applyN(Object[] args) throws Throwable
  {
    checkArgCount(this, args.length);
    CallContext ctx = CallContext.getInstance();
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
        if (method.match(ctx, args) == 0)
	  return method.applyV(ctx);
      }
    throw new WrongType(this, WrongType.ARG_UNKNOWN, null);
  }

  public int isApplicable(Type[] args)
  {
    int best = -1;
    for (int i = count;  --i >= 0; )
      {
        MethodProc method = methods[i];
        int result = method.isApplicable(args);
        if (result == 1)
          return 1;
        if (result == 0)
          best = 0;
      }
    return best;
  }

  public int match (CallContext ctx, Object[] args)
  {
    if (count == 1)
      return methods[0].match(ctx, args);
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
	int code = method.match(ctx, args);
	if (code == 0)
	  {
	    ctx.ivalue1 = i;
	    return 0;
	  }
      }
    return NO_MATCH;
  }

  public Object applyV(CallContext ctx) throws Throwable
  {
    return methods[ctx.ivalue1].applyV(ctx);
  }

  /** Create a GenericProc from one or more methods, plus properties. */
  public static GenericProc make (Object[] args)
  {
    int alen = args.length;
    int mlen = 0;
    GenericProc result = new GenericProc();
    for (int i = 0;  i < alen;  i++)
      {
	Object arg = args[i];
	if (arg instanceof Keyword)
	  {
	    String name = ((Keyword) arg).getName();
	    Object value = args[++i];
	    if (name == "name")
	      result.setName(value.toString());
	    else if (name == "method")
	      result.add((MethodProc) value);
	    else
	      result.setProperty(name, value);
	  }
	else
	  result.add((MethodProc) arg);
      }
    return result;
  }
}
