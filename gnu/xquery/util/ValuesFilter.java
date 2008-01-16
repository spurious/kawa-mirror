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
import gnu.kawa.functions.NumberCompare;
import gnu.kawa.functions.ValuesMap;

public class ValuesFilter extends MethodProc implements CanInline, Inlineable
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
    if (result instanceof Values)
      result = ((Values) result).canonicalize();
    if (result instanceof Number)
      {
        if (result instanceof IntNum)
          return IntNum.compare((IntNum) result, count) == 0;
        if (result instanceof Double || result instanceof Float
            || result instanceof gnu.math.DFloNum)
          return ((Number) result).doubleValue() == (double) count;
        if (result instanceof Long || result instanceof Integer
            || result instanceof Short || result instanceof Byte)
          return count == ((Number) result).longValue();
        // Non-optimal for BigDecimal and BigInteger.  FIXME.
        return NumberCompare.applyWithPromotion(Compare.TRUE_IF_EQU,
                                                IntNum.make(count),
                                                result);
      }
    return BooleanValue.booleanValue(result);
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
    // The filter procedures takes 3 arguments is last() is needed,
    // or 2 arguments if inline has determined we don't need last().
    int pmax = proc.maxArgs();
    for (int i = 0;  i < count;  i++)
      {
	it = values.nextPos(it);
	Object dot = values.getPosPrevious(it);
	int pos = kind == 'R' ? (count - i) : (i + 1);
	IntNum posObj = IntNum.make(pos);
	Object pred_res = pmax == 2 ? proc.apply2(dot, posObj)
          : proc.apply3(dot, posObj, countObj);
	if (matches(pred_res, pos))
	  out.writeObject(dot);
      }
    return;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined); // FIXME - be smarter about type propagation
    Expression[] args = exp.getArgs();
    Expression exp2 = args[1];
    LambdaExp lexp2;
    if (! (exp2 instanceof LambdaExp)
	|| (lexp2 = (LambdaExp) exp2).min_args != 3
	|| lexp2.max_args != 3)
      return exp;

    exp.setType(args[0].getType());

    Compilation parser = walker.getCompilation();

    Declaration dotArg = lexp2.firstDecl();
    Declaration posArg = dotArg.nextDecl();
    Declaration lastArg = posArg.nextDecl();

    lexp2.setInlineOnly(true);
    lexp2.returnContinuation = exp;

    // Splice out lastArg
    lexp2.remove(posArg, lastArg);
    lexp2.min_args = 2;
    lexp2.max_args = 2;

    if (! lastArg.getCanRead() && kind != 'R')
      {
        // Don't need to do anything more - lastArg is not needed.
        return exp;
      }

    parser.letStart();
    Expression seq = args[0];
    Type seqType;
    Method sizeMethod;
    if (kind == 'P')
      {
        seqType = seq.getType();
        sizeMethod = Compilation.typeValues.getDeclaredMethod("countValues", 1);
      }
    else
      {
        seqType = SortNodes.typeSortedNodes;
        seq = new ApplyExp(SortNodes.sortNodes, new Expression [] {seq});
        sizeMethod = CoerceNodes.typeNodes.getDeclaredMethod("size", 0);
      }
    Declaration sequence = parser.letVariable("sequence", seqType, seq);
    parser.letEnter();

    Expression pred = lexp2.body;
    if (kind == 'R')
      {
        Declaration posIncoming = new Declaration(null, Type.int_type);
	Expression init
	  = new ApplyExp(AddOp.$Mn,
			 new Expression[] {
			   new ReferenceExp(lastArg),
			   new ReferenceExp(posIncoming)});
	init
	  = new ApplyExp(AddOp.$Pl,
			 new Expression[] {
			   init,
			   new QuoteExp(IntNum.one())});
        LetExp let = new LetExp(new Expression[] { init });
        lexp2.replaceFollowing(dotArg, posIncoming);
        let.add(posArg);
        let.body = pred;
        pred = let;
      }

    Type predType = lexp2.body.getType();
    if (predType != XDataType.booleanType) // Overly conservative, but simple.
      pred = new ApplyExp(matchesMethod,
                          new Expression[] { pred,
                                             new ReferenceExp(posArg) });
    pred = new IfExp(pred,
                     new ReferenceExp(dotArg),
                     QuoteExp.voidExp);
    lexp2.body = pred;

    ApplyExp doMap
      = new ApplyExp(ValuesMap.valuesMapWithPos,
		     new Expression[] { lexp2,
					new ReferenceExp(sequence) });
    doMap.setType(dotArg.getType());
    lexp2.returnContinuation = doMap;

    Expression lastInit = new ApplyExp(sizeMethod,
                                       new Expression[] {
                                         new ReferenceExp(sequence)});

    LetExp let2 = new LetExp(new Expression[] { lastInit });
    let2.add(lastArg);
    let2.body = ValuesMap.valuesMapWithPos.inline(doMap, walker, true);


    return parser.letDone(let2);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    Expression exp1 = args[0];
    Expression exp2 = args[1];
    if (target instanceof IgnoreTarget)
      {
        exp1.compile(comp, target);
        exp2.compile(comp, target);
        return;
      }
    if (! (exp2 instanceof LambdaExp))
      {
        ApplyExp.compile(exp, comp, target);
        return;
      }

    if (! (target instanceof ConsumerTarget))
      {
	ConsumerTarget.compileUsingConsumer(exp, comp, target);
	return;
      }
    // Should also handle SeriesTarget - later? FIXME

    // At this point, we don't depend on last().
    LambdaExp lexp2 = (LambdaExp) exp2;
    ValuesMap.compileInlined(lexp2, exp1, 1, matchesMethod, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    // Needlessly conservative, but it shouldn't matter, since this
    // shouldn't be called if the ApplyExp.setType has been done.
    return Type.pointer_type;
  }

  public static final ValuesFilter forwardFilter = new ValuesFilter('F');
  public static final ValuesFilter reverseFilter = new ValuesFilter('R');
  public static final ValuesFilter exprFilter = new ValuesFilter('P');
  public static final ClassType typeValuesFilter
    = ClassType.make("gnu.xquery.util.ValuesFilter");
  public static final Method matchesMethod
    = typeValuesFilter.getDeclaredMethod("matches", 2);
}
