// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.mapping.*;
import java.io.*;

/** A QName with namespace nodes [and future optional type annotation]. */

public class XName extends SName implements Externalizable
{
  NamespaceBinding namespaceNodes;

  public XName ()
  {
  }

  public XName (Symbol qname, NamespaceBinding namespaceNodes)
  {
    super(qname, "");
    this.namespaceNodes = namespaceNodes;
  }

  public XName (Symbol qname, String prefix, NamespaceBinding namespaceNodes)
  {
    super(qname, prefix);
    this.namespaceNodes = namespaceNodes;
  }

  public XName (SName name, NamespaceBinding namespaceNodes)
  {
    this(name.symbol, name.getPrefix(), namespaceNodes);
  }

  public final NamespaceBinding getNamespaceNodes () { return namespaceNodes; }
  public final void setNamespaceNodes (NamespaceBinding nodes)
  { this.namespaceNodes = nodes; }

  /** @deprecated */
  public final String getLocalName()
  {
    return getLocalPart();
  }

  String lookupNamespaceURI (String prefix)
  {
    for (NamespaceBinding ns = namespaceNodes;  ns != null;  ns = ns.next)
      {
	if (prefix == ns.prefix)
	  return ns.uri;
      }
    return null;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    super.writeExternal(out);
    out.writeObject(namespaceNodes);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    super.readExternal(in);
    namespaceNodes = (NamespaceBinding) in.readObject();
  }

  /* #ifdef JAXP-1.3 */
  // public Object readResolve() throws ObjectStreamException
  // {
  //   Namespace ns = symbol.getNamespace();
  //   if (ns instanceof NamespacePair)
  //     {
  //       NamespacePair np = (NamespacePair) ns;
  //       return new XName(Symbol.make(np.realNamespace, symbol.getName()),
  //                        ns.getName(), namespaceNodes);
  //     }
  //   return this;
  // }
  /* #endif */
}
