// Copyright (c) 2010  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;

/** Specialization of {@code ExpVisitor<R,D>} to {@code R=Expression}. */

public abstract class ExpExpVisitor<D> extends ExpVisitor<Expression,D>
{
  protected Expression update (Expression exp, Expression r)
  {
    return r;
  }

  protected Expression defaultValue(Expression r, D d)
  {
    return r;
  }
}
