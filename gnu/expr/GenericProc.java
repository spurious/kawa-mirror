// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;

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
    methods[count++] = method;

    int n = method.minArgs();
    if (n < minArgs)
      minArgs = n;
    n = method.maxArgs();
    if (n == -1 || n > maxArgs)
      maxArgs = n;
  }

  public Object applyN(Object[] args)
  {
    checkArgCount(this, args.length);
    for (int i = count;  --i >= 0; )
      {
        MethodProc method = methods[i];
        Object vars = method.getVarBuffer();
        if (method.match(vars, args) == null)
          return method.applyV(vars);
      }
    throw new WrongType(this, WrongType.ARG_UNKNOWN, null);
  }

  public Object getVarBuffer()
  {
    // First element points to selected method, second to that method's args.
    return new Object[2];
  }

  public int isApplicable(Expression[] args)
  {
    int best = 0;
    for (int i = count;  --i >= 0; )
      {
        MethodProc method = methods[i];
        int result = method.isApplicable(args);
        if (result == 2)
          return 2;
        if (result == 1)
          best = 1;
      }
    return best;
  }

  public RuntimeException match (Object vars, Object[] args)
  {
    RuntimeException ex = null;
    for (int i = count;  --i >= 0; )
      {
        MethodProc method = methods[i];
        Object mvars = method.getVarBuffer();
        ex = method.match(mvars, args);
        if (ex == null)
          {
            ((Object[]) vars)[0] = method;
            ((Object[]) vars)[1] = mvars;
            return null;
          }
      }
    if (count == 1)
      return ex;
    return new WrongType(this, WrongType.ARG_UNKNOWN, null);
  }

  public Object applyV(Object vars)
  {
    Object[] arr = (Object[]) vars;
    return ((MethodProc) arr[0]).applyV(arr[1]);
  }
}
