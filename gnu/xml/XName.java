// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.mapping.*;
import java.io.*;

/** A QName with namespace nodes [and future optional type annotation]. */

public class XName implements Externalizable
		   // Maybe: extends javax.xml.namespace.QName, if JAVA5
{
  Symbol qname;
  NamespaceBinding namespaceNodes;

  public XName ()
  {
  }

  public XName (Symbol qname, NamespaceBinding namespaceNodes)
  {
    this.qname = qname;
    this.namespaceNodes = namespaceNodes;
  }

  public final Symbol getQName () { return qname; }
  public final void setQName (Symbol qname) { this.qname = qname; }
  public final NamespaceBinding getNamespaceNodes () { return namespaceNodes; }
  public final void setNamespaceNodes (NamespaceBinding nodes)
  { this.namespaceNodes = nodes; }

  public final String getNamespaceURI()
  {
    return qname.getNamespaceURI();
  }

  public final String getLocalName()
  {
    return qname.getName();
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
    out.writeObject(qname);
    out.writeObject(namespaceNodes);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    qname = (Symbol) in.readObject();
    namespaceNodes = (NamespaceBinding) in.readObject();
  }
}
