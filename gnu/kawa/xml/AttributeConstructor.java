// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.lists.*;

public class AttributeConstructor extends NodeConstructor
implements Externalizable
{
  /** XML source name - e.g. "PREFIX:LOCAL". */
  String sname;

  Symbol qname;

  /** Get name is XML source syntax - i.e. "PREFIX:LOCAL". */
  public String getXmlName() { return sname; }

  public Symbol getQName() { return qname; }
  public void setQName(Symbol qname) { this.qname = qname; }

  public final String getNamespaceURI() { return qname.getNamespaceURI(); }

  public final String getLocalName() { return qname.getLocalName(); }

  public final String getPrefix()
  {
    int colon = sname.indexOf(':');
    if (colon <= 0)  return null;
    return sname.substring(0, colon);
  }

  public static AttributeConstructor make(String sname, Symbol qname)
  {
    AttributeConstructor result = new AttributeConstructor();
    result.sname = sname;
    result.qname = qname;
    return result;
  }

  public static AttributeConstructor make(String sname, String namespaceURI, String localName)
  {
    AttributeConstructor result = new AttributeConstructor();
    result.sname = sname;
    result.qname = Symbol.make(namespaceURI, localName);
    return result;
  }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = pushNodeContext(ctx);
    try
      {
	out.beginAttribute(sname, qname);
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
	out.endAttribute();
      }
    finally
      {
	popNodeContext(saved, ctx);
      }
  }

  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    Variable consumer = ((ConsumerTarget) target).getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    code.emitLoad(consumer);
    comp.compileConstant(sname, Target.pushObject);
    comp.compileConstant(qname, Target.pushObject);
    code.emitInvokeInterface(beginAttributeMethod);
    for (int i = 0;  i < nargs;  i++)
      compileChild(args[i], comp, target);
    code.emitLoad(consumer);
    code.emitInvokeInterface(endAttributeMethod);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  public String toString()
  {
    return "#<AttributeConstructor "+sname+" :: "+qname+'>';
  }


  static final Method beginAttributeMethod
    = Compilation.typeConsumer.getDeclaredMethod("beginAttribute", 2);
  static final Method endAttributeMethod
    = Compilation.typeConsumer.getDeclaredMethod("endAttribute", 0);

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
