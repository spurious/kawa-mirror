// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import java.io.*;
import gnu.mapping.Symbol;

/** Matches an attribute name pattern.
 * FIXME:  ElementType and AttributeType should both inherit
 * from a common NamedNodeType class. */

public class AttributeType extends NodeType
implements TypeValue, Externalizable, AttributePredicate
{
  Symbol qname;

  public static AttributeType make (String namespaceURI, String localName)
  {
    return new AttributeType(Symbol.make(namespaceURI, localName));
  }

  public static AttributeType make (Symbol qname)
  {
    return new AttributeType(qname);
  }

  public AttributeType(Symbol qname)
  {
    this(null, qname);
  }

  public AttributeType(String name, Symbol qname)
  {
    super(name != null && name.length() > 0 ? name
	    : "ATTRIBUTE "+qname+" (*)");
    this.qname = qname;
  }

  public Type getImplementationType()
  {
    return Type.pointer_type;
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

  public boolean isInstancePos (AbstractSequence seq, int ipos)
  {
    int kind = seq.getNextKind(ipos);
    if (kind == Sequence.ATTRIBUTE_VALUE)
      return isInstance(seq, ipos, seq.getNextTypeObject(ipos));
    if (kind == Sequence.OBJECT_VALUE)
      return isInstance(seq.getPosNext(ipos));
    return false;
  }

  public boolean isInstance(AbstractSequence seq, int ipos, Object attrType)
  {
    String namespaceURI = qname.getNamespaceURI();
    String localName = qname.getLocalName();
    String curNamespaceURI;
    String curLocalName;
    /* #ifdef JAXP-1.3 */
    // if (attrType instanceof javax.xml.namespace.QName)
    //   {
    //     javax.xml.namespace.QName qtype
    //       = (javax.xml.namespace.QName) attrType;
    //     curNamespaceURI = qtype.getNamespaceURI();
    //     curLocalName = qtype.getLocalPart();
    //   }
    /* #else */
    if (attrType instanceof SName)
      {
        SName qtype = (SName) attrType;
        curNamespaceURI = qtype.getNamespaceURI();
        curLocalName = qtype.getLocalPart();
      }
    /* #endif */
    else if (attrType instanceof Symbol)
      {
	Symbol qname = (Symbol) attrType;
	curNamespaceURI = qname.getNamespaceURI();
	curLocalName = qname.getLocalName();
      }
    else
      {
	curNamespaceURI = "";
	curLocalName = attrType.toString().intern();  // FIXME
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
    SeqPosition pos = NodeType.coerceOrNull(obj, ATTRIBUTE_OK);
    if (pos == null)
      return null;
    Object curName = pos.getNextTypeObject();
    String curNamespaceURI;
    String curLocalName;
    /* #ifdef JAXP-1.3 */
    // if (curName instanceof javax.xml.namespace.QName)
    //   {
    //     javax.xml.namespace.QName qtype
    //       = (javax.xml.namespace.QName) curName;
    //     curNamespaceURI = qtype.getNamespaceURI();
    //     curLocalName = qtype.getLocalPart();
    //   }
    /* #else */
    if (curName instanceof SName)
      {
        SName qtype = (SName) curName;
        curNamespaceURI = qtype.getNamespaceURI();
        curLocalName = qtype.getLocalPart();
      }
    /* #endif */
    else if (curName instanceof Symbol)
      {
	Symbol qname = (Symbol) curName;
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

  protected void emitCoerceOrNullMethod(Variable incoming, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    code.emitPushString(qname.getNamespaceURI());
    code.emitPushString(qname.getLocalName());
    code.emitInvokeStatic(coerceOrNullMethod);
  }

  public static final ClassType typeAttributeType
    = ClassType.make("gnu.kawa.xml.AttributeType");
  static final Method coerceMethod
    = typeAttributeType.getDeclaredMethod("coerce", 3);
  static final Method coerceOrNullMethod
    = typeAttributeType.getDeclaredMethod("coerceOrNull", 3);

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
    qname = (Symbol) in.readObject();
  }

  public String toString ()
  {
    return "AttributeType " + qname;
  }
}
