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
    RelativeStepFilter filter = new RelativeStepFilter(out);
    for (int pos = 1; pos <= count; pos++)
      {
	it = values.nextPos(it);
	Object dot = values.getPosPrevious(it);
	proc.check3(dot, IntNum.make(pos), countObj, ctx);
        Values.writeValues(ctx.runUntilValue(), filter);
      }
    filter.finish();
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

    exp2 = lexp2.body;

    // 'A' - atomic; 'N' - nodes; 'S' - pre-sorted nodes; ' ' - unknown.
    char expectedKind = ' ';
    // Future optimizations:  [FIXME]
    // If E2 is a child:: step, then change E1 to SortNodes(E1),
    // and set expectedKind to 'S'.
    // If type(E1) <= node()?, and E2 is a AxisStep,
    // then set expectedType to 'S'.
    // Otherwise, if E2 is an AxisStep, set expectedType to 'N'.

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
    if (expectedKind == 'A' || expectedKind == 'S')
      return lastLet;
    Procedure sort;
    if (expectedKind == 'N')
      sort = SortNodes.sortNodes;
    else
      // FIXME should support generating code that doesn't write everything
      // to a temporary before using RelativeStepFilter.  See SortNodes.
      sort = new PrimProcedure("gnu.xquery.util.RelativeStep",
                               "maybeSortNodes$X", 2);
    return new ApplyExp(sort,
                        new Expression[] { comp.letDone(lastLet) });
  }

  public static void maybeSortNodes$X (Object arg, CallContext ctx)
  {
    Consumer out = ctx.consumer;
    RelativeStepFilter filter = new RelativeStepFilter(out);
    Values.writeValues(arg, filter);
    filter.finish();
  }
}
