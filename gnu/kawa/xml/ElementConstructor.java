// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.xml.*;
import gnu.lists.*;

public class ElementConstructor extends NodeConstructor
implements Externalizable
{
  /** XML source name - e.g. "PREFIX:LOCAL". */
  String sname;

  Object type;

  /** Get name is XML source syntax - i.e. "PREFIX:LOCAL". */
  public String getXmlName()
  {
    return sname;
  }

  public Symbol getQName()
  {
    if (type instanceof XName)
      return ((XName) type).getQName();
    else
      return (Symbol) type;
  }

  public void setQName(Symbol qname)
  {
    if (type instanceof XName)
      ((XName) type).setQName(qname);
    else
      type = qname;
  }

  public NamespaceBinding getNamespaceNodes ()
  {
    return type instanceof XName ? ((XName) type).getNamespaceNodes() : null;
  }

  public void setNamespaceNodes (NamespaceBinding bindings)
  {
    if (type instanceof XName)
      ((XName) type).setNamespaceNodes(bindings);
    else
      {
	type = new XName((Symbol) type, bindings);
      }
  }

  public final String getNamespaceURI()
  { return getQName().getNamespaceURI(); }

  public final String getLocalName()
  { return getQName().getLocalName(); }

  public final String getPrefix()
  {
    int colon = sname.indexOf(':');
    if (colon <= 0)  return null;
    return sname.substring(0, colon);
  }

  public static ElementConstructor make(String sname, Symbol qname)
  {
    ElementConstructor result = new ElementConstructor();
    result.sname = sname;
    result.type = qname;
    return result;
  }

  public static ElementConstructor make(String sname, String namespaceURI, String localName)
  {
    ElementConstructor result = new ElementConstructor();
    result.sname = sname;
    result.type = Symbol.make(namespaceURI, localName);
    return result;
  }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = pushNodeContext(ctx);
    try
      {
	out.beginGroup(sname, type);
	Object endMarker = Symbol.UNBOUND;
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
    finally
      {
	popNodeContext(saved, ctx);
      }
  }

  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    Variable consumer = target.getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    code.emitLoad(consumer);
    comp.compileConstant(sname, Target.pushObject);
    comp.compileConstant(type, Target.pushObject);
    code.emitInvokeInterface(beginGroupMethod);
    for (int i = 0;  i < nargs;  i++)
      compileChild(args[i], comp, target);
    code.emitLoad(consumer);
    comp.compileConstant(sname, Target.pushObject);
    code.emitInvokeInterface(endGroupMethod);
  }

  public String toString()
  {
    return "#<ElementConstructor "+sname+" :: "+type+'>';
  }


  static final Method beginGroupMethod
    = Compilation.typeConsumer.getDeclaredMethod("beginGroup", 2);
  static final Method endGroupMethod
    = Compilation.typeConsumer.getDeclaredMethod("endGroup", 1);

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(sname);
    out.writeObject(type);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    sname = (String) in.readObject();
    type = in.readObject();
  }

}
