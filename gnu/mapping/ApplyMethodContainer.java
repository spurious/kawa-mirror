// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** An object that may have methods that provide the "action" needed
 * by an ApplyMethodProc. */

public interface ApplyMethodContainer
{
  public Object apply0(ApplyMethodProc proc);
  public Object apply1(ApplyMethodProc proc, Object arg1);
  public Object apply2(ApplyMethodProc proc, Object arg1, Object arg2);
  public Object apply3(ApplyMethodProc proc,
                       Object arg1, Object arg2, Object arg3);
  public Object apply4(ApplyMethodProc proc,
                       Object arg1, Object arg2, Object arg3, Object arg4);
  public Object applyN(ApplyMethodProc proc, Object[] args);
}
