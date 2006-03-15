// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.math.IntNum;
import gnu.kawa.functions.*;

/** Implements XPath path expression.
 * The XPath expression E1/E2 is compiled into:
 * (relative-step E1 (lambda (dot position last) E2)).
 */

public class RelativeStep extends MethodProc implements CanInline
{
  public static final RelativeStep relativeStep = new RelativeStep();

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object arg = ctx.getNextArg();
    Object next = ctx.getNextArg();
    Procedure proc = (Procedure) next;
    Consumer out = ctx.consumer;
    SortedNodes nodes = new SortedNodes();
    ctx.consumer = nodes;
    IntNum countObj;
    Nodes values;
    if (arg instanceof Nodes)
      values = (Nodes) arg;
    else
      {
	values = new Nodes();
	Values.writeValues(arg, values);
      }
    int count = values.size();
    int it = 0;
    countObj = IntNum.make(count);
    for (int pos = 1; pos <= count; pos++)
      {
	it = values.nextPos(it);
	Object dot = values.getPosPrevious(it);
	proc.check3(dot, IntNum.make(pos), countObj, ctx);
	ctx.runUntilDone();
      }
    nodes.consume(out);
    ctx.consumer = out;
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    Expression exp1 = args[0];
    Expression exp2 = args[1];
    LambdaExp lexp2;
    Compilation comp = walker.getCompilation();
    if (! (exp2 instanceof LambdaExp)
        // The following optimization breaks when interpreting, because
        // then CoerceToNodes may not work.
        || ! comp.mustCompile
	|| (lexp2 = (LambdaExp) exp2).min_args != 3
	|| lexp2.max_args != 3)
      return exp;

    Declaration dotArg = lexp2.firstDecl();
    Declaration posArg = dotArg.nextDecl();
    Declaration lastArg = posArg.nextDecl();
    // Splice out the "last" argument - we'll move it out.
    // The remaining two arguments are suitable for a ValuesMap.
    posArg.setNext(lastArg.nextDecl());
    lastArg.setNext(null);
    lexp2.min_args = 2;
    lexp2.max_args = 2;

    comp.letStart();
    // New to "coerce" to Values - or a NodeList.
    ClassType typeNodes = CoerceNodes.typeNodes;
    ClassType typeSortedNodes = SortNodes.typeSortedNodes;
    Declaration sequence
      = comp.letVariable("sequence", typeNodes,
			   new ApplyExp(CoerceNodes.coerceNodes,
					new Expression [] {args[0]}));
    comp.letEnter();
    Method sizeMethod = typeNodes.getDeclaredMethod("size", 0);
    Expression lastInit
      =  new ApplyExp(sizeMethod,
		      new Expression[] {new ReferenceExp(sequence)});
    LetExp lastLet = new LetExp(new Expression[] { lastInit });
    lastLet.addDeclaration(lastArg);
    ValuesMap valuesMapWithPos = ValuesMap.valuesMapWithPos;
    Expression[] mapArgs
      = new Expression[] { lexp2, new ReferenceExp(sequence) };
    lastLet.body
      = valuesMapWithPos.inline(new ApplyExp(valuesMapWithPos, mapArgs),
				walker);
    return new ApplyExp(SortNodes.sortNodes,
			new Expression[] { comp.letDone(lastLet) });
  }
}
