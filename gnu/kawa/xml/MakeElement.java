// Copyright (c) 2001, 2003, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.xml.*;

public class MakeElement extends NodeConstructor
{
  public static final MakeElement makeElement = new MakeElement();

  public int numArgs() { return 0xFFFFF001; }

  /** Optional tag.  If non-null, the element tag is this value,
   * rather than the first parameter. */
  public Object tag;

  public int copyNamespacesMode = NodeTree.COPY_NAMESPACES_PRESERVE;

  private boolean handlingKeywordParameters;

  /** Should {@code KEYWORD: EXPRESSION} be mapped to an
   * attribute constructor? */
  public boolean isHandlingKeywordParameters ()
  {
    return handlingKeywordParameters;
  }

  public void setHandlingKeywordParameters (boolean value)
  {
    handlingKeywordParameters = value;
  }

  NamespaceBinding namespaceNodes;

  public NamespaceBinding getNamespaceNodes ()
  {
    return namespaceNodes;
  }

  public void setNamespaceNodes (NamespaceBinding bindings)
  {
    namespaceNodes = bindings;
  }

  public static Symbol getTagName (ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    if (args.length > 0)
      {
	Expression arg0 = args[0];
	if (arg0 instanceof QuoteExp)
	  {
	    Object val = ((QuoteExp) arg0).getValue();
	    if (val instanceof Symbol)
	      return (Symbol) val;
	  }
      }
    return null;
  }

  public static void beginGroup(Consumer out, Object qname,
                                int copyNamespacesMode,
				NamespaceBinding namespaceNodes)
  {
    String name;
    XName type;
    if (qname instanceof Symbol)
      {
	type = new XName((Symbol) qname, namespaceNodes);
	name = type.toString();
      }
    else
      {
	name = qname.toString();
	type = new XName(Symbol.make("", name, ""), namespaceNodes);
      }
    out.beginGroup(name, type);
    if (out instanceof NodeTree)
      ((NodeTree) out).copyNamespacesMode = copyNamespacesMode;
  }

  public static void beginGroup(Consumer out, Object qname,
                                int copyNamespacesMode)
  {
    String name;
    Symbol type;
    if (qname instanceof Symbol)
      {
	type = (Symbol) qname;
	name = type.toString();
      }
    else
      {
	name = qname.toString();
	type = Symbol.make("", name, "");
      }
    out.beginGroup(name, type);
  }

  public static void endGroup(Consumer out, Object type)
  {
    String name;
    if (type instanceof Symbol)
      name = ((Symbol) type).getLocalName();
    else
      name = type.toString();
    out.endGroup(name);
  }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = pushNodeContext(ctx);
    try
      {
	Object type = tag != null ? tag : ctx.getNextArg();
	if (namespaceNodes != null)
	  beginGroup(out, type, copyNamespacesMode, namespaceNodes);
	else
	  beginGroup(out, type, copyNamespacesMode);
	Object endMarker = Special.dfault;
	for (;;)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
	    if (arg instanceof Consumable)
	      ((Consumable) arg).consume(out);
	    else
	      ctx.writeValue(arg);
            // Handling Keyword values is actually done by the Consumer.
            if (isHandlingKeywordParameters())
              out.endAttribute();
	  }
	endGroup(out, type);
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
    code.emitDup();
    int i;
    if (tag == null)
      {
        args[0].compile(comp, Target.pushObject);
        i = 1;
      }
    else
      {
        comp.compileConstant(tag, Target.pushObject);
        i = 0;
      }
    code.emitDup(1, 1); // dup_x1
    // Stack:  consumer, tagtype, consumer, tagtype
    code.emitPushInt(copyNamespacesMode);
    if (namespaceNodes != null)
      {
	comp.compileConstant(namespaceNodes, Target.pushObject);
	code.emitInvokeStatic(beginGroupMethod4);
      }
    else
      code.emitInvokeStatic(beginGroupMethod3);
    for (;  i < nargs;  i++)
      {
        compileChild(args[i], comp, target);
        if (isHandlingKeywordParameters())
          {
            code.emitLoad(consumer);
            code.emitInvokeInterface(MakeAttribute.endAttributeMethod);
          }
      }
    code.emitInvokeStatic(endGroupMethod);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  static final ClassType typeMakeElement
    = ClassType.make("gnu.kawa.xml.MakeElement");
  static final Method beginGroupMethod3
    = typeMakeElement.getDeclaredMethod("beginGroup", 3);
  static final Method beginGroupMethod4
    = typeMakeElement.getDeclaredMethod("beginGroup", 4);
  static final Method endGroupMethod
    = typeMakeElement.getDeclaredMethod("endGroup", 2);
}
