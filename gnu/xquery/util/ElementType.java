// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;

public class ElementType extends NodeType implements TypeValue
{
  QName qname;

  public ElementType(QName qname)
  {
    super("ELEMENT "+qname+" (*)");
    this.qname = qname;
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitPushString(qname.getNamespaceURI());
    code.emitPushString(qname.getLocalName());
    code.emitInvokeStatic(coerceMethod);
  }

  public Object coerceFromObject (Object obj)
  {
    return coerce(obj, qname.getNamespaceURI(), qname.getLocalName());
  }

  public static SeqPosition coerceOrNull (Object obj,
				    String namespaceURI, String localName)
  {
    SeqPosition pos = NodeType.coerce(obj);
    if (pos.sequence == null)
      return null;
    Object curName = pos.getNextTypeObject();
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
      return pos;
    return null;
  }

  public static SeqPosition coerce (Object obj,
				    String namespaceURI, String localName)
  {
    SeqPosition pos = coerceOrNull(obj, namespaceURI, localName);
    if (pos == null)
      throw new ClassCastException();
    return pos;
  }

  public void emitTestIf(Variable incoming, Declaration decl, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    code.emitPushString(qname.getNamespaceURI());
    code.emitPushString(qname.getLocalName());
    code.emitInvokeStatic(coerceOrNullMethod);
    if (decl != null)
      {
	code.emitDup();
	decl.compileStore(comp);
      }
    code.emitIfNotNull();
  }

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	CodeAttr code = comp.getCode();
	if (incoming != null)
	  code.emitLoad(incoming);
	code.emitPushString(qname.getNamespaceURI());
	code.emitPushString(qname.getLocalName());
	code.emitInvokeStatic(coerceOrNullMethod);
	if (ctarget.trueBranchComesFirst)
	  code.emitGotoIfCompare1(ctarget.ifFalse, 198); // ifnull
	else
	  code.emitGotoIfCompare1(ctarget.ifTrue, 199); // ifnonnull
	ctarget.emitGotoFirstBranch(code);
      }
    else
      gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
  }

  public static final ClassType typeElementType
    = ClassType.make("gnu.xquery.util.ElementType");
  static final Method coerceMethod
    = typeElementType.getDeclaredMethod("coerce", 3);
  static final Method coerceOrNullMethod
    = typeElementType.getDeclaredMethod("coerceOrNull", 3);
}
