// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** An object that may have methods that provide the "action" needed
 * by an CpsMethodProc. */

public interface CpsMethodContainer
{
  public void apply(CpsMethodProc proc, CallContext context);
}
