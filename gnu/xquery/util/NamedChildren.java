// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.ClassMethods;

public class NamedChildren extends CpsProcedure implements Inlineable
{
  public static final NamedChildren namedChildren = new NamedChildren();
  
  public int numArgs() { return 0x3003; }

  public static void namedChildren (String namespaceURI, String localName,
				    TreeList tlist, int index,
				    Consumer consumer)
    throws Throwable
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

  static final Class[] noClasses = {};

  public static void namedChildren (String namespaceURI, String localName, Object node, Consumer consumer)
    throws Throwable
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
    else if (namespaceURI == "")
      Values.writeValues(getNamedProperty(node, localName), consumer);
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
    throws Throwable
  {
    AbstractSequence seq = position.sequence;

    if (seq == null)
      {
	if (namespaceURI == "")
	  {
	    if (position.ipos > 1)
	      return false;
	    position.xpos = getNamedProperty(position.xpos, localName);
	    return position.xpos != Values.empty;
	  }
	return false;
      }
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
    if (pos.sequence == null)
      pos.ipos++;
    else
      pos.sequence.gotoNext(pos, 0);
  }

  static final SeqPosition nullPosition
  = new SeqPosition(LList.Empty, 0, false);

  public static SeqPosition gotoFirstChild(SeqPosition pos)
  {
    TreeList seq = (TreeList) pos.sequence;
    if (seq == null)
      {
	if (pos.ipos == 0)
	  {
	    pos.ipos = 1;
	    return pos;
	  }
	return nullPosition;
      }
    int child = seq.gotoChildrenStart(pos.ipos >> 1);
    if (child < 0)
      return nullPosition;
    return SeqPosition.make(seq, child << 1, null);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();


    if (nargs == 3 && target instanceof ConsumerTarget
	&& ! (args[0] instanceof ReferenceExp))
      {
	code.pushScope();
	Variable newConsumer = code.addLocal(typeNamedChildrenFilter);
	args[1].compile(comp, comp.typeString);  // Push namespaceURI.
	args[2].compile(comp, comp.typeString);  // Push localName.
	code.emitLoad(((ConsumerTarget) target).getConsumerVariable());
	code.emitInvokeStatic(makeNamedChildrenFilterMethod);
	code.emitStore(newConsumer);
	args[0].compileWithPosition(comp, new ConsumerTarget(newConsumer));
	code.popScope();
	return;
      }

    if (nargs == 3
	&& (target instanceof SeriesTarget
	    	    || target instanceof ConsumerTarget
))
      {
	Variable child = target instanceof SeriesTarget ? code.addLocal(typeSeqPosition) : null;
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
	pathTarget.value
	  = code.addLocal(target instanceof SeriesTarget ? typeSeqPosition
			  : Type.pointer_type);
	args[0].compile(comp, pathTarget);

	if (code.reachableHere())
	  code.emitGoto(pathTarget.done);
	pathTarget.function.define(code);
	code.pushType(retAddrType);
	code.emitStore(retAddr);
	Label nextChildLoopTop = null;
	if (target instanceof SeriesTarget)
	  {
	    code.emitLoad(pathTarget.value);
	    code.emitInvokeStatic(gotoFirstChildMethod);
	    code.emitStore(child);
	    nextChildLoopTop = new Label(code);
	    nextChildLoopTop.define(code);
	    code.emitLoad(child);
	  }
	if (namespaceURIVar == null)
	  args[1].compile(comp, comp.typeString);
	else
	  code.emitLoad(namespaceURIVar);
	if (localNameVar == null)
	  args[2].compile(comp, comp.typeString);
	else
	  code.emitLoad(localNameVar);
	if (target instanceof ConsumerTarget)
	  {
	    code.emitLoad(pathTarget.value);
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
    = ClassType.make("gnu.xquery.util.NamedChildren");
  static final ClassType typeSeqPosition = NodeType.nodeType;
  static final Method getNamedChildMethod
    = typeNamedChildren.getDeclaredMethod("getNamedChild", 3);
  static final Method namedChildrenMethod
    = typeNamedChildren.getDeclaredMethod("namedChildren", 4);
  static final Method gotoFirstChildMethod
    = typeNamedChildren.getDeclaredMethod("gotoFirstChild", 1);
  static final Method gotoNextMethod
    = typeNamedChildren.getDeclaredMethod("gotoNext", 1);
}
