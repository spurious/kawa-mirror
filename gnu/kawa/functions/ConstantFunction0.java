// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.expr.*;

/** A 0-argument function that returns a constant value.
 * Used for false() and true() in XQuery. */

public class ConstantFunction0 extends Procedure0 implements CanInline
{
  final Object value;

  public ConstantFunction0(String name, Object value)
  {
    super(name);
    this.value = value;
  }

  public Object apply0() { return value; }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    int nargs = exp.getArgCount();
    if (nargs != 0 && walker != null)
      {
	String message = WrongArguments.checkArgCount(this, nargs);
	return walker.noteError(message);
      }
    return new QuoteExp(value);
  }
}
