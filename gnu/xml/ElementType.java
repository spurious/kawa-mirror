// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;

/** Name and type information for an element tag in a given namespace.
 * If DTDs or XML Schemas are implemented, the "type" of an element
 * should be a sub-class of the class, but for now, all we do is handle
 * name, though in a namespace-aware way. */

public class ElementType
{
  /** Chain together in same ElementType hash bucket. */
  protected ElementType chain;

  /** The namespace URI if it exists, else null.
   * This String is assumed to be interned. */
  protected String namespaceURI;

  /** The local name of a qualified name.
   * If namespaceURI is null, this is the fully-qualified name.
   * This string is assumed to be interned. */
  protected String localName;

  public ElementType(String localName, String namespaceURI)
  {
    this.localName = localName;
    this.namespaceURI = namespaceURI;
  }

  public String getNamespaceURI()
  {
    return namespaceURI;
  }

  public String getLocalName()
  {
    return namespaceURI == null ? null : localName;
  }

  public static boolean same(ElementType e1, ElementType e2)
  {
    return e1.localName == e2.localName && e1.namespaceURI == e2.namespaceURI;
  }

  public boolean equals(Object o)
  {
    if (! (o instanceof ElementType))
      return false;
    ElementType e2 = (ElementType) o;
    return localName == e2.localName && namespaceURI == e2.namespaceURI;
  }

  public String toString()
  {
    return "<name=" + localName + " uri=" + namespaceURI + '>';
  }

  public int hashCode()
  {
    return localName.hashCode();
  }
}
