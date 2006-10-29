// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.mapping.*;
import java.io.*;

/** A QName with namespace nodes [and future optional type annotation]. */

public class XName extends Symbol implements Externalizable
{
  NamespaceBinding namespaceNodes;

  public XName ()
  {
  }

  public XName (Symbol symbol, NamespaceBinding namespaceNodes)
  {
    super(symbol.getNamespace(), symbol.getName());
    this.namespaceNodes = namespaceNodes;
  }

  /** Namespace nodes associated with an element.
   * These are in inverse document/parse order.
   */
  public final NamespaceBinding getNamespaceNodes () { return namespaceNodes; }
  public final void setNamespaceNodes (NamespaceBinding nodes)
  { this.namespaceNodes = nodes; }

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

  public static boolean isNameStart(char ch)
  {
    return Character.isLetter(ch) || ch == '_';
  }

  public static boolean isNamePart(char ch)
  {
    return Character.isUnicodeIdentifierPart(ch) || ch == '-' || ch == '.';
  }

  public static boolean isName (String value)
  {
    return isName(value, false);
  }

  public static boolean isName (String value, boolean prohibitColon)
  {
    int len = value.length();
    if (len == 0 || ! XName.isNameStart(value.charAt(0)))
      return false;
    for (int i = len;  --i > 0; )
      {
        char ch = value.charAt(i);
        if (! XName.isNamePart(ch) && (ch != ':' || prohibitColon))
          return false;
      }
    return true;
  }
}
