// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import java.io.*;

public class ElementType extends NodeType
implements TypeValue, Externalizable, GroupPredicate
{
  QName qname;

  public ElementType(String namespaceURI, String localName)
  {
    this(QName.make(namespaceURI, localName));
  }

  public ElementType(QName qname)
  {
    this(null, qname);
  }

  public ElementType(String name, QName qname)
  {
    super(name != null && name.length() > 0 ? name
	    : "ELEMENT "+qname+" (*)");
    this.qname = qname;
  }

  public final String getNamespaceURI () { return qname.getNamespaceURI(); }
  public final String getLocalName () { return qname.getLocalName(); }

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

  public boolean isInstance(AbstractSequence seq, int ipos, Object xpos)
  {
    return seq.getNextKind(ipos, xpos) == Sequence.GROUP_VALUE
      && isInstance(seq, ipos, xpos, seq.getNextTypeObject(ipos, xpos));
  }

  public boolean isInstance(AbstractSequence seq, int ipos, Object xpos,
			    Object groupType)
  {
    String namespaceURI = qname.getNamespaceURI();
    String localName = qname.getLocalName();
    String curNamespaceURI;
    String curLocalName;
    if (groupType instanceof QName)
      {
	QName qname = (QName) groupType;
	curNamespaceURI = qname.getNamespaceURI();
	curLocalName = qname.getLocalName();
      }
    else
      {
	curNamespaceURI = "";
	curLocalName = groupType.toString().intern();  // FIXME
      }
    return ((localName == curLocalName || localName == null)
	    && (namespaceURI == curNamespaceURI || namespaceURI == null));
  }

  public boolean isInstance (Object obj)
  {
    return  coerceOrNull(obj, qname.getNamespaceURI(),qname.getLocalName())
      != null;
  }

  public static SeqPosition coerceOrNull (Object obj,
				    String namespaceURI, String localName)
  {
    SeqPosition pos = NodeType.coerceOrNull(obj, GROUP_OK);
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
    = ClassType.make("gnu.kawa.xml.ElementType");
  static final Method coerceMethod
    = typeElementType.getDeclaredMethod("coerce", 3);
  static final Method coerceOrNullMethod
    = typeElementType.getDeclaredMethod("coerceOrNull", 3);

  public void writeExternal(ObjectOutput out) throws IOException
  {
    String name = getName();
    out.writeUTF(name == null ? "" : name);
    out.writeObject(qname);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    String name = in.readUTF();
    if (name.length() > 0)
      setName(name);
    qname = (QName) in.readObject();
  }
}
