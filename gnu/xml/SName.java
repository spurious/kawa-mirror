// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
/* #ifdef JAXP-1.3 */
// import javax.xml.namespace.QName;
/* #endif */
import gnu.mapping.*;
import java.io.*;

/**
 * Represents an "expanded QName" - i.e. a (namepaceURI, localPart)-pair.
 * Also stores namespace prefix used in lexical form, but prefix
 * is ignored for <code>equals</code>.
 * If configured to use JAXP 1.3, then this class extends
 * <code>javax.xml.namespace.QName</code>.
 */

public class SName
  /* #ifdef JAXP-1.3 */
  // extends QName
  /* #endif */
  implements Externalizable
{
  Symbol symbol;
  /* #ifndef JAXP-1.3 */
  String prefix;
  /* #endif */

  public SName ()
  {
    /* #ifdef JAXP-1.3 */
    // super("");
    /* #endif */
  }

  public SName (Symbol symbol, String prefix)
  {
    /* #ifdef JAXP-1.3 */
    // super(symbol.getNamespaceURI(), symbol.getName(), prefix);
    /* #endif */
    /* #ifndef JAXP-1.3 */
    this.prefix = prefix;
    /* #endif */
    this.symbol = symbol;
  }

  public SName (String namespaceURI, String localPart, String prefix)
  {
    this(Symbol.make(namespaceURI, localPart), prefix);
  }

  public static SName make (String namespaceURI, String localPart,
			    String prefix)
  {
    return new SName(namespaceURI, localPart, prefix);
  }

  /* #ifndef JAXP-1.3 */
  public final String getNamespaceURI()
  {
    return symbol.getNamespaceURI();
  }

  public final String getLocalPart()
  {
    return symbol.getName();
  }

  public final String getPrefix ()
  {
    return prefix;
  }
  /* #endif */

  public final Symbol getSymbol () { return symbol; }
  public final void setSymbol (Symbol qname) { this.symbol = qname; }

  public final boolean equals (SName obj)
  {
    return symbol == obj.symbol;
  }

  /* #ifndef JAXP-1.3 */
  public final boolean equals (Object obj)
  {
    return obj instanceof SName && symbol == ((SName) obj).symbol;
  }

  public final int hashCode ()
  {
    return symbol.hashCode();
  }
  /* #endif */

  public String toString ()
  {
    String localPart = getLocalPart();
    String prefix = getPrefix();
    if (prefix.length() > 0)
      return prefix + ':' + localPart;
    String namespaceURI = getNamespaceURI();
    if (namespaceURI.length() == 0)
      return localPart;
    return "{" + namespaceURI + '}' + localPart;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(symbol);
    out.writeUTF(getPrefix());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    symbol = (Symbol) in.readObject();
    /* #ifndef JAXP-1.3 */
    prefix = in.readUTF();
    /* #endif */
    // Kludge because we cannot set QName's field.
    /* #ifdef JAXP-1.3 */
    // String prefix = in.readUTF();
    // if (prefix == null || prefix.length() == 0)
    //   return;
    // NamespacePair np = new NamespacePair(symbol.getNamespace(), prefix);
    // symbol = new Symbol(symbol.getName(), np);
    /* #endif */
  }

  /* #ifdef JAXP-1.3 */
  // public Object readResolve() throws ObjectStreamException
  // {
  //   Namespace ns = symbol.getNamespace();
  //   if (ns instanceof NamespacePair)
  //     {
  //       NamespacePair np = (NamespacePair) ns;
  //       return new SName(Symbol.make(np.realNamespace, symbol.getName()),
  //                         ns.getName());
  //     }
  //   return this;
  // }
  /* #endif */
}

class NamespacePair extends Namespace
{
  Namespace realNamespace;
  public NamespacePair (Namespace realNamespace, String prefix)
  {
    super(prefix, 0);
    this.realNamespace = realNamespace;
  }
}
