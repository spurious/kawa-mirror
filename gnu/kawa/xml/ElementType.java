// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
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

  public ElementType(String namespaceURI, String localName)
  {
    this(Symbol.make(namespaceURI, localName));
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
    return seq.getNextKind(ipos) == Sequence.GROUP_VALUE
      && isInstance(seq, ipos, seq.getNextTypeObject(ipos));
  }

  public boolean isInstance(AbstractSequence seq, int ipos, Object groupType)
  {
    String namespaceURI = qname.getNamespaceURI();
    String localName = qname.getLocalName();
    String curNamespaceURI;
    String curLocalName;
    if (groupType instanceof Symbol)
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

  public static SeqPosition coerceOrNull (Object obj,
				    String namespaceURI, String localName)
  {
    SeqPosition pos = NodeType.coerceOrNull(obj, GROUP_OK);
    if (pos == null)
      return null;
    Object curName = pos.getNextTypeObject();
    String curNamespaceURI;
    String curLocalName;
    if (curName instanceof Symbol)
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
