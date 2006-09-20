// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.expr.*;

/** Representation of XQuery's Extension Expression.
 * The expression:
 * (# QName PragmaContents #) ... { Expr }
 * is translated by the parser into:
 * (HandleExtension QName "PragmaContents"  ... (lambda () Expr))
 */

public class HandleExtension extends ProcedureN implements CanInline
{
  public static final HandleExtension handleExtension = new HandleExtension();

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    Expression last = args[args.length-1];
    if (last instanceof LambdaExp)
      return ((LambdaExp) last).body;
    return exp;
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    Object last = args[args.length-1];
    return ((Procedure) last).match0(ctx);
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    Object last = args[args.length-1];
    ((Procedure) last).apply(ctx);
  }

  public Object applyN (Object[] args) throws Throwable
  {
    return ((Procedure) args[args.length-1]).apply0();
  }
}
