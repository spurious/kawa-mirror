// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import java.io.*;
import gnu.mapping.Symbol;

public class ElementType extends NodeType
implements TypeValue, Externalizable, GroupPredicate
{
  Symbol qname;

  /** An element type for match by name.
   * @param localName if null matches any local name; otherwise must
   *  be intered, and matches by identity.
   * @param namespaceURI full name of namespace, or null for any namespace. */
  public static ElementType make (String namespaceURI, String localName)
  {
    return new ElementType(Symbol.make(namespaceURI, localName));
  }

  public ElementType(Symbol qname)
  {
    this(null, qname);
  }

  public ElementType(String name, Symbol qname)
  {
    super(name != null && name.length() > 0 ? name
	    : "ELEMENT "+qname+" (*)");
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
    if (kind == Sequence.GROUP_VALUE)
      return isInstance(seq, ipos, seq.getNextTypeObject(ipos));
    if (kind == Sequence.OBJECT_VALUE)
      return isInstance(seq.getPosNext(ipos));
    return false;
  }

  public boolean isInstance(AbstractSequence seq, int ipos, Object groupType)
  {
    String namespaceURI = qname.getNamespaceURI();
    String localName = qname.getLocalName();
    String curNamespaceURI;
    String curLocalName;
    /* #ifdef JAXP-1.3 */
    // if (groupType instanceof javax.xml.namespace.QName)
    // {
    //   javax.xml.namespace.QName qtype
    //     = (javax.xml.namespace.QName) groupType;
    //   curNamespaceURI = qtype.getNamespaceURI();
    //   curLocalName = qtype.getLocalPart();
    // }
    /* #endif */
    /* #ifndef JAXP-1.3 */
    if (groupType instanceof SName)
     {
       SName qtype = (SName) groupType;
       curNamespaceURI = qtype.getNamespaceURI();
       curLocalName = qtype.getLocalPart();
     }
    /* #endif */
    else if (groupType instanceof Symbol)
      {
	Symbol qname = (Symbol) groupType;
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

  public static KElement coerceOrNull (Object obj,
				       String namespaceURI, String localName)
  {
    KElement pos = (KElement) NodeType.coerceOrNull(obj, GROUP_OK);
    if (pos == null)
      return null;
    Object curName = pos.getNextTypeObject();
    String curNamespaceURI;
    String curLocalName;
    /* #ifdef JAXP-1.3 */
    // if (curName instanceof javax.xml.namespace.QName)
    // {
    //   javax.xml.namespace.QName qtype
    //     = (javax.xml.namespace.QName) curName;
    //   curNamespaceURI = qtype.getNamespaceURI();
    //   curLocalName = qtype.getLocalPart();
    // }
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

  public static KElement coerce (Object obj,
				 String namespaceURI, String localName)
  {
    KElement pos = coerceOrNull(obj, namespaceURI, localName);
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
    qname = (Symbol) in.readObject();
  }

  public String toString ()
  {
    return "ElementType " + qname;
  }
}
