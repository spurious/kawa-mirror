// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;

/**
 * A function that can be called in a xpath expression.
 * Each builtin or used-defined functions will be an instance
 * of XPathFunction.  An XPathContext will point to a hashtable that maps
 * functions names to XPathFunction instances.

 * XPathFunction is logically abstract, but not technically so.
 * To implement a function, you must override either the applyN method (easy)
 * or the apply method (harder, but gives you control
 * over argument evaluation, and may thus be more efficient).
 */

public class XPathFunction
{
  String name;
  int expected;

  public void apply(XPathContext context, int argCount, XPath expr, int pc, int resultType)
  {
    expr.checkArgs(this, argCount, expected);
    pc -= expr.code[pc];
    Object[] args = new Object[argCount];
    for (int i = 0;  i < argCount;  i++)
      {
	pc += expr.code[pc];
	args[i] = expr.eval(context, pc);
	pc++;
      }
    Object result = applyN(args);
    expr.result(context, result, resultType);
  }

  public Object applyN(Object[] args)
  {
    throw new Error("unimplemented function " + this);
  }
}
