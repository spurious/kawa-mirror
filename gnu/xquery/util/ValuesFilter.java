// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.math.IntNum;
import gnu.kawa.functions.AddOp;
import gnu.kawa.functions.ValuesMap;

public class ValuesFilter extends MethodProc implements CanInline
{
  /** 'F' if following a ForwardStep; 'R' if following a ReverseStep;
   * 'P' if following a PrimaryExpr. */
  char kind;

  public ValuesFilter (char kind)
  {
    this.kind = kind;
  }

  public static ValuesFilter get (char kind)
  {
    if (kind == 'F')  return forwardFilter;
    else if (kind == 'R')  return reverseFilter;
    else return exprFilter;
  }

  /** 2 if last() is needed (implicit if kind=='R');
   * 1 if position() is needed;
   * 0 otherwise. */
  int last_or_position_needed = 2;

  public int numArgs() { return 0x2002; }

  static public boolean matches(Object result, long count)
  {
    if (result instanceof Boolean)
      return ((Boolean) result).booleanValue();
    if (result instanceof Number)
      return count == ((Number) result).longValue();
    if (result instanceof String)
      return result.toString().length() > 0;
    if (result instanceof KNode)
      return true;
    if (result instanceof Values)
      {
	Values values = (Values) result;
	int index = 0;
	for (;;)
	  {
	    int next = values.nextDataIndex(index);
	    if (next < 0)
	      return false;
	    if (matches(values.getPosNext(index << 1), count))
	      return true;
	    index = next;
	  }
      }
    if (result instanceof TreeList)
      return ! ((TreeList) result).isEmpty();
    throw new Error("unimplemented condition type:"+result.getClass().getName()); // FIXME
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object arg = ctx.getNextArg();
    Procedure proc = (Procedure) ctx.getNextArg();
    Consumer out = ctx.consumer;
    Values values;
    if (kind != 'P')
      {
	SortedNodes nodes = new SortedNodes();
	Values.writeValues(arg, nodes);
	values = nodes;
      }
    else if (arg instanceof Values)
      values = (Values) arg;
    else
      {
	IntNum one = IntNum.one();
	if (matches(proc.apply3(arg, one, one), 1))
	  out.writeObject(arg);
	return;
      }
    int count = values.size();
    int it = 0;
    IntNum countObj = IntNum.make(count);
    for (int i = 0;  i < count;  i++)
      {
	it = values.nextPos(it);
	Object dot = values.getPosPrevious(it);
	int pos = kind == 'R' ? (count - i) : (i + 1);
	IntNum posObj = IntNum.make(pos);
	Object pred_res = proc.apply3(dot, posObj, countObj);
	if (matches(pred_res, pos))
	  out.writeObject(dot);
      }
    return;
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    Expression exp2 = args[1];
    LambdaExp lexp2;
    if (! (exp2 instanceof LambdaExp)
	|| (lexp2 = (LambdaExp) exp2).min_args != 3
	|| lexp2.max_args != 3)
      return exp;

    if (kind == 'P') // FIXME
      return exp;

    Compilation parser = walker.getCompilation();

    parser.letStart();
    ClassType typeSortedNodes = SortNodes.typeSortedNodes;
    Declaration sequence
      = parser.letVariable("sequence", typeSortedNodes,
			   new ApplyExp(SortNodes.sortNodes,
					new Expression [] {args[0]}));
    parser.letEnter();
    parser.letStart();
    Method sizeMethod = CoerceNodes.typeNodes.getDeclaredMethod("size", 0);
    Declaration lastDecl
      = parser.letVariable("last", Type.int_type,
			   new ApplyExp(sizeMethod,
					new Expression[] {
					  new ReferenceExp(sequence)}));
    parser.letEnter();

    LambdaExp mapLambda = new LambdaExp(2);
    Declaration dotDecl = mapLambda.addDeclaration("dot");
    Declaration atDecl = mapLambda.addDeclaration("position", Type.int_type);

    Declaration posDecl = atDecl;

    if (kind == 'R')
      {
	parser.letStart();
	Expression init
	  = new ApplyExp(AddOp.$Mn,
			 new Expression[] {
			   new ReferenceExp(lastDecl),
			   new ReferenceExp(posDecl)});
	init
	  = new ApplyExp(AddOp.$Pl,
			 new Expression[] {
			   init,
			   new QuoteExp(IntNum.one())});
	posDecl = parser.letVariable("pos", Type.int_type, init);
	parser.letEnter();
      }

    Expression applyPredicate = new ApplyExp(lexp2,
					     new Expression[] { 
					       new ReferenceExp(dotDecl),
					       new ReferenceExp(posDecl),
					       new ReferenceExp(lastDecl)});
    Expression body = new IfExp(new ApplyExp(matchesMethod,
					    new Expression[] {
					      applyPredicate,
					      new ReferenceExp(posDecl) }),
				new ReferenceExp(dotDecl),
				QuoteExp.voidExp);
    if (kind == 'R')
      body = parser.letDone(body);
    mapLambda.body = body;

    ApplyExp doMap
      = new ApplyExp(ValuesMap.valuesMapWithPos,
		     new Expression[] { mapLambda,
					new ReferenceExp(sequence) });
    body = ValuesMap.valuesMapWithPos.inline(doMap, walker);
    return parser.letDone(parser.letDone(body));
  }

  public static final ValuesFilter forwardFilter = new ValuesFilter('F');
  public static final ValuesFilter reverseFilter = new ValuesFilter('R');
  public static final ValuesFilter exprFilter = new ValuesFilter('P');
  public static final ClassType typeValuesFilter
    = ClassType.make("gnu.xquery.util.ValuesFilter");
  public static final Method matchesMethod
    = typeValuesFilter.getDeclaredMethod("matches", 2);
}
