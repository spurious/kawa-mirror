// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.ClassMethods;

public class NamedChildren extends CpsProcedure implements Inlineable
{
  public static final NamedChildren namedChildren = new NamedChildren();
  
  public int numArgs() { return 0x2002; }

  public static void namedChildren (NodePredicate type,
				    TreeList tlist, int index,
				    Consumer consumer)
    throws Throwable
  {
    int child = tlist.gotoChildrenStart(index);
    if (child < 0)
      return;
    TreePosition pos = Focus.getCurrent();
    pos.push(tlist, child << 1);
    for (;;)
      {
	if (! getNamedChild(pos, type))
	  break;
	int ichild = tlist.posToDataIndex(pos.ipos);
	int next = tlist.nextNodeIndex(ichild, -1 >>> 1);
	if (ichild == next)
	  next = tlist.nextDataIndex(ichild);
	if (next == -1)
	  break;
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(tlist, pos.ipos);
	else
	  tlist.consumeIRange(ichild, next, consumer);
	pos.ipos = next << 1;
      }
    pos.pop();
  }

  static final Class[] noClasses = {};

  public static void namedChildren (NodePredicate type, Object node, Consumer consumer)
    throws Throwable
  {
    if (node instanceof TreeList)
      {
	namedChildren(type, (TreeList) node, 0, consumer);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  {
	    TreeList tlist = (TreeList) pos.sequence;
	    namedChildren(type, atlist,
			  tlist.posToDataIndex(pos.ipos), consumer);
	  }
      }
    else if (type instanceof ElementType
	     && ((ElementType) type).getNamespaceURI() == "")
      Values.writeValues(getNamedProperty(node,((ElementType) type).getLocalName()),
			 consumer);
  }

  public static String getPropertyName(String name)
  {
    StringBuffer methodName = new StringBuffer(100);
    methodName.append("get");
    int nameLength = name.length();
    boolean upcase = true;
    for (int i = 0;  i < nameLength;  i++)
      {
	char ch = name.charAt(i);
	if (ch == '-')
	  upcase = true;
	else
	  {
	    if (upcase)
	      {
		ch = Character.toTitleCase(ch);
		upcase = false;
	      }
	    methodName.append(ch);
	  }
      }
    return methodName.toString();
  }

  public static Object getNamedProperty (Object node, String name)
    throws Throwable
  {
    // Look for a property with a matching name.
    String methodName = getPropertyName(name)
      .intern();

    ClassType nodeType = (ClassType) Type.make(node.getClass());
    PrimProcedure[] methods
      = ClassMethods.getMethods(nodeType, methodName, 0, 0,
				Interpreter.getInterpreter());
    Type[] atypes = { nodeType };
    long count = ClassMethods.selectApplicable(methods, atypes);
    if (count == (1L << 32L))
      return methods[0].apply1(node);
    else if (count != 0)
      throw new IllegalArgumentException("no property named "+name+" in "+nodeType);
    else
      return Values.empty;
  }

  public void apply (CallContext ctx)  throws Throwable
  {
    Consumer consumer = ctx.consumer;
    Object node = ctx.getNextArg();
    NodePredicate predicate = (NodePredicate) ctx.getNextArg();
    ctx.lastArg();
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index << 1);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      namedChildren(predicate, tlist.getPosNext(index << 1), consumer);
	    else
	      namedChildren(predicate, tlist, index, consumer);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      namedChildren(predicate, node, consumer);
  }

  public static boolean getNamedChild(SeqPosition position, NodePredicate type)
    throws Throwable
  {
    AbstractSequence seq = position.sequence;
    /* FIXME
    if (seq == null)
      {
	if (type instanceof ElementType
	    && ((ElementType) type).getNamespaceURI() == "")
	  {
	    if (position.ipos > 1)
	      return false;
	    position.xpos
	      = getNamedProperty(position.xpos,
				 ((ElementType) type).getLocalName());
	    return position.xpos != Values.empty;
	  }
	return false;
      }
    */
    while (position.hasNext())
      {
	int ipos = position.getPos();
	if (type.isInstancePos(seq, ipos))
	  return true;
	if (seq instanceof TreeList)
	  {
	    TreeList tlist = (TreeList) seq;
	    int index = tlist.posToDataIndex(ipos);
	    int next = tlist.nextNodeIndex(index, -1 >>> 1);
	    if (next != index)
	      {
		position.ipos = next << 1;
		continue;
	      }
	  }
	position.gotoNext();
      }
    return false;
  }


  public static void gotoNext(SeqPosition pos)
  {
    AbstractSequence seq = pos.sequence;
    if (seq == null)
      pos.ipos++;
    else
      {
	if (seq instanceof TreeList)
	  {
	    TreeList tlist = (TreeList) seq;
	    int index = tlist.posToDataIndex(pos.ipos);
	    int next = tlist.nextNodeIndex(index, -1 >>> 1);
	    if (next != index)
	      {
		pos.ipos = next << 1;
		return;
	      }
	  }
	pos.gotoNext();
      }
  }

  static final SeqPosition nullPosition
  = new SeqPosition(LList.Empty, 0, false);

  public static SeqPosition gotoFirstChild(SeqPosition pos)
  {
    AbstractSequence seq = pos.sequence;
    if (seq == null)
      {
	if (pos.ipos == 0)
	  {
	    pos.ipos = 1;
	    return pos;
	  }
	return nullPosition;
      }
    int child = seq.firstChildPos(pos.ipos);
    if (child == 0)
      return nullPosition;
    return SeqPosition.make(seq, child);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    Object type;

    if (nargs == 2 && target instanceof ConsumerTarget
	&& ! (args[0] instanceof ReferenceExp)
	&& args[1] instanceof QuoteExp
	&& (type = ((QuoteExp) args[1]).getValue()) instanceof ElementType)
      {
	code.pushScope();
	Variable newConsumer = code.addLocal(typeNamedChildrenFilter);
	ElementType etype = (ElementType) type;
	comp.compileConstant(etype.getNamespaceURI());
	comp.compileConstant(etype.getLocalName());
	code.emitLoad(((ConsumerTarget) target).getConsumerVariable());
	code.emitInvokeStatic(makeNamedChildrenFilterMethod);
	code.emitStore(newConsumer);
	args[0].compileWithPosition(comp, new ConsumerTarget(newConsumer));
	code.popScope();
	return;
      }

    if (nargs == 2
	&& (target instanceof SeriesTarget
	    || target instanceof ConsumerTarget))
      {
	Variable child = target instanceof SeriesTarget ? code.addLocal(typeSeqPosition) : null;
	Type retAddrType = Type.pointer_type;
	SeriesTarget pathTarget = new SeriesTarget();
	pathTarget.scope = code.pushScope();
	Variable retAddr = code.addLocal(retAddrType);
	Variable predicateVar;
	if (args[1] instanceof QuoteExp)
	  predicateVar = null;
	else
	  {
	    predicateVar = code.addLocal(typeNodePredicate);
	    args[1].compile(comp, typeNodePredicate);
	    code.emitStore(predicateVar);
	  }

	pathTarget.function = new Label(code);
	pathTarget.done = new Label(code);
	Type pathType = target instanceof SeriesTarget ? typeSeqPosition
	  : Type.pointer_type;
	pathTarget.param = new Declaration(code.addLocal(pathType));
	args[0].compile(comp, pathTarget);

	if (code.reachableHere())
	  code.emitGoto(pathTarget.done);
	pathTarget.function.define(code);
	code.pushType(retAddrType);
	code.emitStore(retAddr);
	Label nextChildLoopTop = null;
	if (target instanceof SeriesTarget)
	  {
	    pathTarget.param.load(comp);
	    code.emitInvokeStatic(gotoFirstChildMethod);
	    code.emitStore(child);
	    nextChildLoopTop = new Label(code);
	    nextChildLoopTop.define(code);
	    code.emitLoad(child);
	  }
	if (predicateVar == null)
	  args[1].compile(comp, typeNodePredicate);
	else
	  code.emitLoad(predicateVar);
	if (target instanceof ConsumerTarget)
	  {
	    pathTarget.param.load(comp);
	    code.emitLoad(((ConsumerTarget) target).getConsumerVariable());
	    code.emitInvokeStatic(namedChildrenMethod);
	    code.emitRet(retAddr);
	  }
	else
	  {
	    code.emitInvokeStatic(getNamedChildMethod);
	    Label ok = new Label(code);
	    code.emitGotoIfIntNeZero(ok);
	    code.emitRet(retAddr);
	    ok.define(code);
	    code.emitLoad(child);
	    SeriesTarget starget = (SeriesTarget) target;
	    starget.compileFromStackSimple(comp, typeSeqPosition);
	    code.emitLoad(child);
	    code.emitInvokeStatic(gotoNextMethod);
	    code.emitGoto(nextChildLoopTop);
	  }
	code.popScope();
	pathTarget.done.define(code);
	return;
      }
    ApplyExp.compile(exp, comp, target);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  static final ClassType typeNamedChildrenFilter
    = ClassType.make("gnu.xml.NamedChildrenFilter");
  static final Method makeNamedChildrenFilterMethod
  = typeNamedChildrenFilter.getDeclaredMethod("make", 3);
  static final ClassType typeNamedChildren
    = ClassType.make("gnu.kawa.xml.NamedChildren");
  static final ClassType typeNodePredicate
    = ClassType.make("gnu.lists.NodePredicate");
  static final ClassType typeSeqPosition = NodeType.nodeType;
  static final Method getNamedChildMethod
    = typeNamedChildren.getDeclaredMethod("getNamedChild", 2);
  static final Method namedChildrenMethod
    = typeNamedChildren.getDeclaredMethod("namedChildren", 3);
  static final Method gotoFirstChildMethod
    = typeNamedChildren.getDeclaredMethod("gotoFirstChild", 1);
  static final Method gotoNextMethod
    = typeNamedChildren.getDeclaredMethod("gotoNext", 1);
}
