// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class NamedChildren extends CpsProcedure implements Inlineable
{
  public static final NamedChildren namedChildren = new NamedChildren();
  
  public int numArgs() { return 0x3003; }

  public static void namedChildren (String namespaceURI, String localName,
				    TreeList tlist, int index,
				    Consumer consumer)
  {
    int child = tlist.gotoChildrenStart(index);
    if (child < 0)
      return;
    SeqPosition pos = SeqPosition.make(tlist, child << 1, null);
    for (;;)
      {
	if (! getNamedChild(pos, namespaceURI, localName))
	  break;
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(tlist,
						      pos.ipos, pos.xpos);
	else
	  {
	    int ichild = pos.ipos >>> 1;
	    int next = tlist.nextDataIndex(ichild);
	    tlist.consumeRange(ichild, next, consumer);
	    pos.ipos = next << 1;
	  }
	tlist.gotoNext(pos, 0);
      }
  }

  public static void namedChildren (String namespaceURI, String localName, Object node, Consumer consumer)
  {
    if (node instanceof TreeList)
      {
	namedChildren(namespaceURI, localName, (TreeList) node, 0, consumer);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  namedChildren(namespaceURI, localName, (TreeList) pos.sequence, pos.ipos >> 1, consumer);
      }
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object node = ctx.getNextArg();
    String namespaceURI = (String) ctx.getNextArg();
    String localName = (String) ctx.getNextArg();
    ctx.lastArg();
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index << 1, null);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      namedChildren(namespaceURI, localName, tlist.getNext(index << 1, null), consumer);
	    else
	      namedChildren(namespaceURI, localName, tlist, index, consumer);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      namedChildren(namespaceURI, localName, node, consumer);
  }

  public static boolean getNamedChild(SeqPosition position,
				      String namespaceURI, String localName)
  {
    AbstractSequence seq = position.sequence;
    for (;;)
      {
	int ipos = position.ipos;
	Object xpos = position.xpos;
	int kind = seq.getNextKind(ipos, xpos);
	if (kind == Sequence.EOF_VALUE)
	  return false;
	if (kind == Sequence.GROUP_VALUE)
	  {
	    Object curName = seq.getNextTypeObject(ipos, xpos);
	    String curNamespaceURI;
	    String curLocalName;
	    if (curName instanceof QName)
	      {
		QName qname = (QName) curName;
		curNamespaceURI = qname.getNamespaceURI();
		curLocalName = qname.getLocalName();
	      }
	    else
	      {
		curNamespaceURI = "";
		curLocalName = curName.toString().intern();  // FIXME
	      }
	    if ((localName == curLocalName || localName == null)
		&& (namespaceURI == curNamespaceURI || namespaceURI == null))
	      return true;
	  }
	seq.gotoNext(position, 0);
      }
  }

  public static void gotoNext(SeqPosition pos)
  {
    pos.sequence.gotoNext(pos, 0);
  }

  static final SeqPosition nullPosition
  = new SeqPosition(LList.Empty, 0, false);

  public static SeqPosition gotoFirstChild(SeqPosition pos)
  {
    TreeList seq = (TreeList) pos.sequence;
    int child = seq.gotoChildrenStart(pos.ipos >> 1);
    if (child < 0)
      return nullPosition;
    return SeqPosition.make(seq, child << 1, null);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;

    if (nargs == 3
	&& (target instanceof SeriesTarget
	    /* || target instanceof ConsumerTarget */))
      {
	CodeAttr code = comp.getCode();

	Variable child = code.addLocal(typeSeqPosition);
	Type retAddrType = Type.pointer_type;
	Variable retAddr = code.addLocal(retAddrType);
	Variable namespaceURIVar, localNameVar;
	if (args[1] instanceof QuoteExp)
	  namespaceURIVar = null;
	else
	  {
	    namespaceURIVar = code.addLocal(comp.typeString);
	    args[1].compile(comp, comp.typeString);
	    code.emitStore(namespaceURIVar);
	  }
	if (args[2] instanceof QuoteExp)
	  localNameVar = null;
	else
	  {
	    localNameVar = code.addLocal(comp.typeString);
	    args[2].compile(comp, comp.typeString);
	    code.emitStore(localNameVar);
	  }

	SeriesTarget pathTarget = new SeriesTarget();
	pathTarget.function = new Label(code);
	pathTarget.done = new Label(code);
	pathTarget.value = code.addLocal(typeSeqPosition);

	args[0].compile(comp, pathTarget);

	if (code.reachableHere())
	  code.emitGoto(pathTarget.done);
	pathTarget.function.define(code);
	code.pushType(retAddrType);
	code.emitStore(retAddr);
	code.emitLoad(pathTarget.value);
	code.emitInvokeStatic(gotoFirstChildMethod);
	code.emitStore(child);
	Label nextChildLoopTop = new Label(code);
	nextChildLoopTop.define(code);
	code.emitLoad(child);
	if (namespaceURIVar == null)
	  args[1].compile(comp, comp.typeString);
	else
	  code.emitLoad(namespaceURIVar);
	if (localNameVar == null)
	  args[2].compile(comp, comp.typeString);
	else
	  code.emitLoad(localNameVar);
	code.emitInvokeStatic(getNamedChildMethod);
	Label ok = new Label(code);
	code.emitGotoIfIntNeZero(ok);
	code.emitRet(retAddr);
	ok.define(code);
	SeriesTarget starget = (SeriesTarget) target;
	code.emitLoad(child);
	starget.compileFromStackSimple(comp, typeSeqPosition);
	code.emitLoad(child);
	code.emitInvokeStatic(gotoNextMethod);
	code.emitGoto(nextChildLoopTop);
	pathTarget.done.define(code);
	return;
      }
    ApplyExp.compile(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  static final ClassType typeNamedChildren
    = ClassType.make("gnu.xquery.util.NamedChildren");
  static final ClassType typeSeqPosition = NodeType.nodeType;
  static final Method getNamedChildMethod
    = typeNamedChildren.getDeclaredMethod("getNamedChild", 3);
  static final Method gotoFirstChildMethod
    = typeNamedChildren.getDeclaredMethod("gotoFirstChild", 1);
  static final Method gotoNextMethod
    = typeNamedChildren.getDeclaredMethod("gotoNext", 1);
}
