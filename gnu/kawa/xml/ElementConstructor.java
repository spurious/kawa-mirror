// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.xml.*;
import gnu.lists.*;

public class ElementConstructor extends CpsProcedure
implements Inlineable, Externalizable
{
  /** XML source name - e.g. "PREFIX:LOCAL". */
  String sname;

  Symbol qname;

  /** Get name is XML source syntax - i.e. "PREFIX:LOCAL". */
  public String getXmlName() { return sname; }

  public Symbol getQName() { return qname; }

  public final String getNamespaceURI() { return qname.getNamespaceURI(); }

  public final String getLocalName() { return qname.getLocalName(); }

  public final String getPrefix()
  {
    int colon = sname.indexOf(':');
    if (colon <= 0)  return null;
    return sname.substring(0, colon);
  }

  public static ElementConstructor make(String sname, Symbol qname)
  {
    ElementConstructor result = new ElementConstructor();
    result.sname = sname.intern();
    result.qname = qname;
    return result;
  }

  public static ElementConstructor make(String sname, String namespaceURI, String localName)
  {
    ElementConstructor result = new ElementConstructor();
    result.sname = sname.intern();
    result.qname = Symbol.make(namespaceURI, localName);
    return result;
  }

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    Object endMarker = Symbol.UNBOUND;
    out.beginGroup(sname, qname);
    for (;;)
      {
	Object arg = ctx.getNextArg(endMarker);
	if (arg == endMarker)
	  break;
	if (arg instanceof Consumable)
	  ((Consumable) arg).consume(out);
	else
	  out.writeObject(arg);
      }
    out.endGroup(sname);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    if (target instanceof ConsumerTarget)
      {
	Variable consumer = ((ConsumerTarget) target).getConsumerVariable();
	Expression[] args = exp.getArgs();
	int nargs = args.length;
	CodeAttr code = comp.getCode();
	
	code.emitLoad(consumer);
	comp.compileConstant(sname, Target.pushObject);
	comp.compileConstant(qname, Target.pushObject);
	code.emitInvokeInterface(beginGroupMethod);
	for (int i = 0;  i < nargs;  i++)
	  args[i].compileWithPosition(comp, target);
	code.emitLoad(consumer);
	comp.compileConstant(sname, Target.pushObject);
	code.emitInvokeInterface(endGroupMethod);
      }
    else if (target instanceof IgnoreTarget)
      ApplyExp.compile(exp, comp, target);
    else
      compileUsingNodeTree(exp, comp, target);
  }

  /** Compile an expression using a fresh NodeTree.
   * Compare with ConsumerTarget.compileUsingConsumer, but creates a NodeTree.
   */
  public static void compileUsingNodeTree(Expression exp,
					  Compilation comp, Target target)
  {
    Method makeMethod = typeNodeTree.getDeclaredMethod("make", 0);
    ConsumerTarget.compileUsingConsumer(exp, comp, target, makeMethod, null);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  public String toString()
  {
    return "#<ElementConstructor "+sname+" :: "+qname+'>';
  }


  static final ClassType typeNodeTree
    = ClassType.make("gnu.xml.NodeTree");

  static final Method beginGroupMethod
    = Compilation.typeConsumer.getDeclaredMethod("beginGroup", 2);
  static final Method endGroupMethod
    = Compilation.typeConsumer.getDeclaredMethod("endGroup", 1);

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(sname);
    out.writeObject(qname);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    sname = ((String) in.readObject()).intern();
    qname = (Symbol) in.readObject();
  }

}
