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

  public static boolean isNameStart(int ch)
  {
    /* #ifdef JAVA5 */
    // return Character.isLetter(ch) || ch == '_';
    /* #else */
    return ch >= 0x10000 || Character.isLetter((char) ch) || ch == '_';
    /* #endif */
  }

  public static boolean isNamePart(int ch)
  {
    /* #ifdef JAVA5 */
    // return Character.isUnicodeIdentifierPart(ch) || ch == '-' || ch == '.';
    /* #else */
    return ch >= 0x10000 || Character.isUnicodeIdentifierPart((char) ch)
      || ch == '-' || ch == '.';
    /* #endif */
  }

  public static boolean isName (String value)
  {
    return isName(value, false);
  }

  public static boolean isName (String value, boolean prohibitColon)
  {
    int len = value.length();
    if (len == 0)
      return false;
    for (int i = 0;  i < len;  )
      {
        boolean first = i == 0;
        int ch = value.charAt(i++);
        if (ch >= 0xD800 && ch < 0xDC00 && i < len)
          ch = (ch - 0xD800) * 0x400 + (value.charAt(i++) - 0xDC00) + 0x10000;
        if (! (first ? XName.isNameStart(ch) : XName.isNamePart(ch)))
          return false;
      }
    return true;
  }
}
