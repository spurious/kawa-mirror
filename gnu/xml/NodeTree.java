// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.kawa.xml.KNode;
import gnu.xml.XName;
import gnu.kawa.xml.MakeText;  // FIXME
import gnu.expr.Keyword; // FIXME

/** Use to represent a Document or Document Fragment, in the XML DOM sense.
 * More compact than traditional DOM, since it uses many fewer objects.
 */

public class NodeTree extends TreeList
{
  public void writePosition(AbstractSequence seq, int ipos)
  {
    seq.consumeNext(ipos, this);
  }

  int gapStartLastAtomic = -1;
  private static final int SAW_KEYWORD = -2;

  /** If v is a node, make a copy of it. */
  public void writeObject(Object v)
  {
    if (v instanceof SeqPosition)
      {
	SeqPosition pos = (SeqPosition) v;
	writePosition(pos.sequence, pos.getPos());
      }
    else if (v instanceof TreeList)
      ((TreeList) v).consume(this);
    else if (v instanceof gnu.expr.Keyword)
      {
        beginAttribute(((Keyword) v).getName(), v);
        gapStartLastAtomic = SAW_KEYWORD;
      }
    // Using super.writeObject would be nice, but there are edge cases.
    // Specifically, atomic nodes with a zero-length string-value.
    // Handling that case correctly and efficiently is left for later.  FIXME.
    // else
    //  super.writeObject(v);
    else
      {
        if (gapStartLastAtomic == gapStart)
          writeChar(' ');
        MakeText.text$C(v, this);  // Atomize.
        gapStartLastAtomic = gapStart;
      }
  }

  public static String
  duplicateAttributeMessage (Symbol attrSymbol, String groupName)
  {
    StringBuffer sbuf = new StringBuffer("duplicate attribute: ");
    String uri = attrSymbol.getNamespaceURI();
    if (uri != null && uri.length() > 0)
      {
        sbuf.append('{');
        sbuf.append('}');
        sbuf.append(uri);
      }
    sbuf.append(attrSymbol.getLocalPart());
    if (groupName != null)
      {
        sbuf.append(" in <");
        sbuf.append(groupName);
        sbuf.append('>');
      }
    return sbuf.toString();
    
  }

  public void endAttribute()
  {
    if (gapStartLastAtomic == SAW_KEYWORD)
      gapStartLastAtomic = -1;
    else
      super.endAttribute();
  }

  protected void writeJoiner ()
  {
    gapStartLastAtomic = -1;
    super.writeJoiner();
  }

  public int nextPos (int position)
  {
    boolean isAfter = (position & 1) != 0;
    int index = posToDataIndex(position);
    int next = nextNodeIndex(index, -1 >>> 1);
    if (next != index)
      return next << 1;
    if (index == data.length)
      return 0;
    return (index << 1) + 3;
  }

  public static NodeTree make ()
  {
    return new NodeTree();
  }

  static int counter;
  int id;

  /** Get/create a new unique number. */
  public int getId()
  {
    if (id == 0)
      id = ++counter;
    return id;
  }

  public int stableCompare (AbstractSequence other)
  {
    if (this == other)
      return 0;
    // If other is also a NodeTree it would be simpler to just compare
    // the results of getId, but if we always did that there is the
    // slight risk that counter could overflow in the case of a
    // long-running program.  So we use system.identityHashCode as
    // the primary "key" and getId only when needed as a tie-breaker.
    int comp = super.stableCompare(other);
    if (comp == 0 && other instanceof NodeTree)
      {
	int id1 = this.getId();
	int id2 = ((NodeTree) other).getId();
	comp = id1 < id2 ? -1 : id1 > id2 ? 1 : 0;
      }
    return comp;
  }

  public SeqPosition getIteratorAtPos(int ipos)
  {
    return KNode.make(this, ipos);
  }

  public String posNamespaceURI (int ipos)
  {
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).getNamespaceURI();
    if (type instanceof Symbol)
      return ((Symbol) type).getNamespaceURI();
    return null;
  }

  public String posPrefix (int ipos)
  {
    String name = getNextTypeName(ipos);
    if (name == null)
      return null;
    int colon = name.indexOf(':');
    return colon < 0 ? null : name.substring(0, colon);
  }

  public String posLocalName (int ipos)
  {
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).getLocalPart();
    if (type instanceof Symbol)
      return ((Symbol) type).getLocalName();
    return getNextTypeName(ipos);
  }

  public boolean posIsDefaultNamespace (int ipos, String namespaceURI)
  {
    throw new Error("posIsDefaultNamespace not implemented");
  }

  public String posLookupNamespaceURI (int ipos, String prefix)
  {
    int kind = getNextKind(ipos);
    if (kind != Sequence.GROUP_VALUE)
      throw new IllegalArgumentException("argument must be an element");
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).lookupNamespaceURI(prefix);
    else
      return null;
  }

  public String posLookupPrefix (int ipos, String namespaceURI)
  {
    throw new Error("posLookupPrefix not implemented");
  }

  public int posFirstChild(int ipos)
  {
    int index = gotoChildrenStart(posToDataIndex(ipos));
    if (index < 0)
      return -1;
    char datum = data[index];
    if (datum == END_GROUP_SHORT || datum == END_GROUP_LONG
	|| datum == END_DOCUMENT)
      return -1;
    return index << 1;
  }

  public boolean posHasAttributes (int ipos)
  {
    int index = gotoAttributesStart(posToDataIndex(ipos));
    if (index < 0)
      return false;
    return index >= 0 && data[index] == BEGIN_ATTRIBUTE_LONG;
  }

  /** Find named attribute.
   * @param namepaceURI need not be interned,
   *   or null which matches any namespace
   * @param localName need not be interned,
   *   or null which matches any local name
   * @return attribute ipos or 0
   */
  public int getAttribute (int parent, String namespaceURI, String localName)
  {
    return getAttributeI(parent,
                         namespaceURI == null ? null : namespaceURI.intern(),
                         localName == null ? null : localName.intern());
  }

  /** Find named attribute.
   * @param namepaceURI an interned String or null which matches any namespace
   * @param localName an interned String, or null which matches any local name
   * @return attribute ipos or 0
   */
  public int getAttributeI (int parent, String namespaceURI, String localName)
  {
    int attr = firstAttributePos(parent);
    for (;;)
      {
        if (attr == 0 || getNextKind(attr) != Sequence.ATTRIBUTE_VALUE)
          return 0;
        if ((localName == null || posLocalName(attr) == localName)
            && (namespaceURI == null || posNamespaceURI(attr) == namespaceURI))
          return attr;
        attr = nextPos(attr);
      }
  }

  /** Return the type-value of the node at the specified position. */
  public Object typedValue (int ipos)
  {
    // FIXME when we support validation.
    StringBuffer sbuf = new StringBuffer();
    stringValue(posToDataIndex(ipos), sbuf);
    // FIXME this should be untypedAtomic.   For now the caller takes care of
    // it by converting a String to UntypedAtomic, but this won't work once
    // we do validation.
    return sbuf.toString();
  }

  /** Get the target of a process-instruction. */
  public String posTarget (int ipos)
  {
    int index = posToDataIndex(ipos);
    if (data[index] != PROCESSING_INSTRUCTION)
      throw new ClassCastException("expected process-instruction");
    return (String) objects[getIntN(index+1)];
  }

  /** Look for matching attribute in ancestor or self.
   * @param namespace namespaceURI (interned) of required attribute
   * @param name localName(interned) of required attribute 
   * @return attribute ipos or 0
   */
  public int ancestorAttribute (int ipos,
                                String namespace, String name)
  {
    for (;;)
      {
        if (ipos == -1)
          return 0;
        int attr = getAttributeI(ipos, namespace, name);
        if (attr != 0)
          return attr;
        ipos = parentPos(ipos);
      }
  }

  public String toString ()
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    XMLPrinter xp = new XMLPrinter(wr);
    consume(xp);
    wr.close();
    return wr.toString();
  }
}
