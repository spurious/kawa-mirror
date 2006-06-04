// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.kawa.xml.KNode;
import gnu.xml.XName;
import gnu.kawa.xml.MakeText;  // FIXME

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
    boolean seenDocument = false;
    int plen = prefix.length();
    for (;;)
      {
	int kind = getNextKind(ipos);
	switch (kind)
	  {
	  case Sequence.DOCUMENT_VALUE:
	    if (seenDocument)
	      return null;
	    ipos = posFirstChild(ipos);
	    // Avoid a loop bouncing between document and its element.
   	    seenDocument = true;
	    continue;
	  case Sequence.GROUP_VALUE:
	    Object type = getNextTypeObject(ipos);
	    if (type instanceof XName)
	      return ((XName) type).lookupNamespaceURI(prefix);
	    else if (type instanceof Symbol)
	      {
		String name = getNextTypeName(ipos);
		if (prefix != null && name != null && name.length() > plen
		    && name.charAt(plen) == ':' && name.startsWith(prefix))
		  return ((Symbol) type).getNamespaceURI();
		else if (prefix == null && name != null
			 && name.indexOf(':') < 0)
		  return ((Symbol) type).getNamespaceURI();
	      }
	    /* ... else fall through ... */
	  default:
	    int parent = parentPos(ipos);
	    if (parent == -1)
	      return null;
	    ipos = parent;
	  }
      }
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

  public String toString ()
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    XMLPrinter xp = new XMLPrinter(wr);
    consume(xp);
    wr.close();
    return wr.toString();
  }
}
