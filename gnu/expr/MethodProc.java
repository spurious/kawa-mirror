// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;

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
   * Returns 0 if no; 2 if yes; 1 if need to check at run-time. */
  public int isApplicable(Expression[] args)
  {
    return 1;
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
}
