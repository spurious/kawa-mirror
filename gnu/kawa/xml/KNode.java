// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import org.w3c.dom.*;
import org.w3c.dom.Document;

public abstract class KNode extends SeqPosition
  implements org.w3c.dom.Node
{

  public KNode (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public static KNode make(NodeTree seq, int ipos)
  {
    int kind = seq.getNextKind(ipos);
    switch (kind)
      {
      case Sequence.GROUP_VALUE:
	return new KElement(seq, ipos);
      case Sequence.ATTRIBUTE_VALUE:
	return new KAttr(seq, ipos);
      case Sequence.DOCUMENT_VALUE:
	return new KDocument(seq, ipos);
      case Sequence.EOF_VALUE:
	return null;
      default:
	return new KText(seq, ipos);
      }
  }

  public static KNode make(NodeTree seq)
  {
    return make(seq, 0);
  }

  public boolean isSupported(String feature, String version)
  {
    return false;
  }

  public abstract short getNodeType ();

  public String getNodeName ()
  {
    return sequence.getNextTypeName(ipos);
  }

  public String getNamespaceURI ()
  {
    return ((NodeTree) sequence).posNamespaceURI(ipos);
  }

  public String getPrefix ()
  {
    return ((NodeTree) sequence).posPrefix(ipos);
  }

  public String getLocalName ()
  {
    return ((NodeTree) sequence).posLocalName(ipos);
  }

  public String getNodeValue()
  {
    StringBuffer sbuf = new StringBuffer();
    NodeTree tlist = (NodeTree) sequence;
    tlist.stringValue(tlist.posToDataIndex(ipos), sbuf);
    return sbuf.toString();
  }

  public Node getParentNode()
  {
    int parent = sequence.parentPos(ipos);
    if (parent == -1)
      return null;
    return make((NodeTree) sequence, parent);
  }

  public Node getPreviousSibling ()
  {
    int parent = sequence.parentPos(ipos);
    if (parent == -1)
      parent = 0;
    int index = ((NodeTree) sequence).posToDataIndex(ipos);
    int child = sequence.firstChildPos(parent);
    int previous = 0;
    for (;;)
      {
	previous = child;
	child = sequence.nextPos(child);
	if (child == 0)
	  break;
	if (((NodeTree) sequence).posToDataIndex(child) == index)
	  break;
      }
    return previous == 0 ? null
      : make((NodeTree) sequence, previous);
  }

  public Node getNextSibling ()
  {
    int next = ((NodeTree) sequence).nextPos(ipos);
    return next == 0 ? null
      : make((NodeTree) sequence, next);
  }

  public Node getFirstChild()
  {
    int child = ((NodeTree) sequence).posFirstChild(ipos);
    return make((NodeTree) sequence, child);
  }

  public Node getLastChild()
  {
    int last = 0;
    int child = sequence.firstChildPos(ipos);
    while (child != 0)
      {
	last = child;
	child = sequence.nextPos(child);
      }
    return last == 0 ? null : make((NodeTree) sequence, last);
  }

  public NodeList getChildNodes ()
  {
    Nodes nodes = new SortedNodes();
    int child = sequence.firstChildPos(ipos);
    while (child != 0)
      {
	nodes.writePosition(sequence, child);
	child = sequence.nextPos(child);
      }
    return nodes;
  }

  /** Not implemented yet. */
  public NodeList getElementsByTagName(String tagname)
  {
    throw new UnsupportedOperationException("getElementsByTagName not implemented yet");
    /*
    Nodes nodes = new SortedNodes();
    int child = sequence.firstChildPos(ipos);
    while (child != 0)
      {
	if (matches)
	  nodes.writePosition(sequence, child);
	child = sequence.nextPos(child);
      }
    return nodes;
    */
  }

  public boolean hasChildNodes()
  {
    return ((NodeTree) sequence).posFirstChild(ipos) >= 0;
  }

  /** Not implemented. */
  public void setNodeValue (String nodeValue)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setNodeValue not supported");
  }

  /** Not implemented. */
  public void setPrefix (String prefix)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setPrefix not supported");
  }

  /** Not implemented. */
   public Node insertBefore(Node newChild, Node refChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "insertBefore not supported");
  }

  /** Not implemented. */
   public Node replaceChild(Node newChild, Node oldChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "replaceChild not supported");
  }

  /** Not implemented. */
   public Node removeChild(Node oldChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "removeChild not supported");
  }

  /** Not implemented. */
   public Node appendChild(Node newChild)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "appendChild not supported");
  }


  /** Only implemented if deep is true. */
  public Node cloneNode(boolean deep)
  {
    if (! deep)
      throw new UnsupportedOperationException("shallow cloneNode not implemented");
    NodeTree tree = new NodeTree();
    ((NodeTree) sequence).consumeNext(ipos, tree);
    return make(tree);
  }

  public Document getOwnerDocument ()
  {
    int kind = sequence.getNextKind(ipos);
    if (kind == Sequence.DOCUMENT_VALUE)
      return new KDocument((NodeTree) sequence, 0);
    return null;
  }

  public void normalize ()
  {
  }

  public boolean hasAttributes ()
  {
    return false;
  }

  public NamedNodeMap getAttributes ()
  {
    throw new UnsupportedOperationException("getAttributes not implemented yet");
  }
}
