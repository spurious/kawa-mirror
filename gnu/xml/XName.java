// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.mapping.*;

/** A QName with namespace nodes [and future optional type annotation]. */

public class XName
{
  Symbol qname;
  NamespaceBinding namespaceNodes;

  public Symbol getQName () { return qname; }

  public final String getNamespaceURI()
  {
    return qname.getNamespaceURI();
  }

  public final String getLocalName()
  {
    return qname.getName();
  }

  String lookupPrefix(String prefix)
  {
    for (NamespaceBinding ns = namespaceNodes;  ns != null;  ns = ns.next)
      {
	if (prefix == ns.prefix)
	  return ns.uri;
      }
    return null;
  }

}
