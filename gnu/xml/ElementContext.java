// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;

/** A factory for managing ElementType instances. */

public class ElementContext
{
  int tableMask = 63;
  ElementType[] table = new ElementType[tableMask+1];

  /**
   * Find or create a matching ElementType.
   * @param localName The local name part of a qualified name.
   * @param uri The Namespace URI the prefix is mapped to.  Must be interned.
   */
  public ElementType lookup(String localName, String uri)
  {
    int hash = localName.hashCode();
    int index = hash & tableMask;
    ElementType old = table[index];
    ElementType el = old;
    for (; el != null;  el = el.chain)
      {
        if (el.namespaceURI == uri && el.localName == localName)
          return el;
      }
    el = new ElementType(localName, uri);
    el.chain = old;
    table[index] = el;
    return el;
  }

}

