// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.math.IntNum;

public class ValuesFilter extends CpsProcedure
{
  /** 'F' if following a ForwardStep; 'R' if following a ReverseStep;
   * 'R' if following a PrimaryExpr. */
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
    if (result instanceof SeqPosition)
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
    throw new Error("unimplemented condition type"); // FIXME
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

    /*
    int index = 0;
    Focus focus = Focus.getCurrent();
    long savePosition = focus.position;
    long position = 0;
    int count = values instanceof Values ? ((Values) values).size() : 1;
    IntNum countObj = IntNum.make(count);
    for (;;)
      {
	int next = Values.nextIndex(values, index);
	if (next < 0)
	  break;
	Object value = Values.nextValue(values, index);
	focus.position = kind == 'R' ? (count - position) : (position + 1);
	position++;
	IntNum posObj = IntNum.make(position);
	if (matches(proc.apply3(value, posObj, countObj), position))
	  out.writeObject(value);
	index = next;
      }
    focus.position = savePosition;
    */
  }

  /*
  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression pred = exp.args[0];
    if (kind != 'P')
      {
	CodeAttr code = comp.getCode();
	Scope scope = code.pushScope();
	Type ntype = SortNodes.typeSortedNodes;
	code.emitNew(ntype);
	code.emitDup(ntype);
	code.emitInvoke(SortNodes.makeSortedNodesMethod);
	Variable nodes = scope.addVariable(code, ntype, "nodes");
	ConsumerTarget ctarget = new ConsumerTarget(nodes);
	code.emitStore(nodes);
	pred.compile(comp, ctarget);

	// emit: int count = nodes.size();
	code.emitLoad(nodes);
	code.emitInvoke("size");
	Variable count = scope.addVariable(code, ntype, "last");
	code.emitStore(count);

	// FIXME:  compile actual loop.

	code.popScope();
      }
    else
      ApplyExp.compile(exp, comp, target);
  }
  */

  public static final ValuesFilter forwardFilter = new ValuesFilter('F');
  public static final ValuesFilter reverseFilter = new ValuesFilter('R');
  public static final ValuesFilter exprFilter = new ValuesFilter('P');
}
