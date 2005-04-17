// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import org.w3c.dom.*;
import org.w3c.dom.Document;
import gnu.mapping.*;

public abstract class KNode extends SeqPosition
  /* #ifdef use:org.w3c.dom.Node */
  // implements org.w3c.dom.Node
  /* #endif */
{

  public KNode (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  /** Convert value to a KNode, returning null if it isn't a node. */
  public static KNode coerce (Object value)
  {
    if (value instanceof KNode)
      return (KNode) value;
    if (value instanceof NodeTree)
      {
	NodeTree ntree = (NodeTree) value;
	return make(ntree, ntree.startPos());
      }
    if (value instanceof SeqPosition
	&& ! (value instanceof TreePosition))
      {
	SeqPosition seqp = (SeqPosition) value;
	if (seqp.sequence instanceof NodeTree)
	  return make((NodeTree) seqp.sequence, seqp.ipos);
      }
    return null;
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
      case Sequence.CDATA_VALUE:
	return new KCDATASection(seq, ipos);
      case Sequence.COMMENT_VALUE:
	return new KComment(seq, ipos);
      case Sequence.PROCESSING_INSTRUCTION_VALUE:
	return new KProcessingInstruction(seq, ipos);
      case Sequence.EOF_VALUE:
	if (! seq.isEmpty())
	  return null;
	// .. else fall through to create an empty text node.
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
    getNodeValue(sbuf);
    return sbuf.toString();
  }

  public void getNodeValue (StringBuffer sbuf)
  {
    NodeTree tlist = (NodeTree) sequence;
    tlist.stringValue(tlist.posToDataIndex(ipos), sbuf);
  }

  public boolean hasChildNodes()
  {
    return ((NodeTree) sequence).posFirstChild(ipos) >= 0;
  }

  public String getTextContent ()
  {
    StringBuffer sbuf = new StringBuffer();
    getTextContent(sbuf);
    return sbuf.toString();
  }

  protected void getTextContent (StringBuffer sbuf)
  {
    // What is the difference between getTextContent and getNodeValue?  FIXME.
    getNodeValue(sbuf);
  }

  /* #ifdef use:org.w3c.dom.Node */
  // public Node getParentNode()
  // {
  //   int parent = sequence.parentPos(ipos);
  //   if (parent == -1)
  //     return null;
  //   return make((NodeTree) sequence, parent);
  // }

  // public Node getPreviousSibling ()
  // {
  //   int parent = sequence.parentPos(ipos);
  //   if (parent == -1)
  //     parent = 0;
  //   int index = ((NodeTree) sequence).posToDataIndex(ipos);
  //   int child = sequence.firstChildPos(parent);
  //   int previous = 0;
  //   for (;;)
  //     {
	// previous = child;
	// child = sequence.nextPos(child);
	// if (child == 0)
	  // break;
	// if (((NodeTree) sequence).posToDataIndex(child) == index)
	  // break;
  //     }
  //   return previous == 0 ? null
  //     : make((NodeTree) sequence, previous);
  // }
  /* #endif */

  /* #ifdef use:org.w3c.dom.Node */
  // public Node getNextSibling ()
  // {
  //   int next = ((NodeTree) sequence).nextPos(ipos);
  //   return next == 0 ? null
  //     : make((NodeTree) sequence, next);
  // }

  // public Node getFirstChild()
  // {
  //   int child = ((NodeTree) sequence).posFirstChild(ipos);
  //   return make((NodeTree) sequence, child);
  // }

  // public Node getLastChild()
  // {
  //   int last = 0;
  //   int child = sequence.firstChildPos(ipos);
  //   while (child != 0)
  //     {
	// last = child;
	// child = sequence.nextPos(child);
  //     }
  //   return last == 0 ? null : make((NodeTree) sequence, last);
  // }

  // public NodeList getChildNodes ()
  // {
  //   Nodes nodes = new SortedNodes();
  //   int child = sequence.firstChildPos(ipos);
  //   while (child != 0)
  //     {
	// nodes.writePosition(sequence, child);
	// child = sequence.nextPos(child);
  //     }
  //   return nodes;
  // }

  // /** Not implemented yet. */
  // public NodeList getElementsByTagName(String tagname)
  // {
  //   throw new UnsupportedOperationException("getElementsByTagName not implemented yet");
  //   /*
  //   Nodes nodes = new SortedNodes();
  //   int child = sequence.firstChildPos(ipos);
  //   while (child != 0)
  //     {
	// if (matches)
	  // nodes.writePosition(sequence, child);
	// child = sequence.nextPos(child);
  //     }
  //   return nodes;
  //   */
  // }
  /* #endif */

  /* #ifdef use:org.w3c.dom.Node */
  // /** Not implemented. */
  // public void setNodeValue (String nodeValue)  throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "setNodeValue not supported");
  // }

  // /** Not implemented. */
  // public void setPrefix (String prefix)  throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "setPrefix not supported");
  // }

  // /** Not implemented. */
  //  public Node insertBefore(Node newChild, Node refChild)
  //    throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "insertBefore not supported");
  // }

  // /** Not implemented. */
  //  public Node replaceChild(Node newChild, Node oldChild)
  //    throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "replaceChild not supported");
  // }

  // /** Not implemented. */
  //  public Node removeChild(Node oldChild)
  //    throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "removeChild not supported");
  // }

  // /** Not implemented. */
  //  public Node appendChild(Node newChild)
  //    throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "appendChild not supported");
  // }

  // /** Not implemented. */
  //  public void setTextContent (String textContent)
  //    throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "setTextContent not supported");
  // }

  // /** Only implemented if deep is true. */
  // public Node cloneNode(boolean deep)
  // {
  //   if (! deep)
  //     throw new UnsupportedOperationException("shallow cloneNode not implemented");
  //   NodeTree tree = new NodeTree();
  //   ((NodeTree) sequence).consumeNext(ipos, tree);
  //   return make(tree);
  // }

  // public Document getOwnerDocument ()
  // {
  //   int kind = sequence.getNextKind(ipos);
  //   if (kind == Sequence.DOCUMENT_VALUE)
  //     return new KDocument((NodeTree) sequence, 0);
  //   return null;
  // }

  // public NamedNodeMap getAttributes ()
  // {
  //   throw new UnsupportedOperationException("getAttributes not implemented yet");
  // }
  /* #endif */

  public void normalize ()
  {
  }

  public boolean hasAttributes ()
  {
    return false;
  }

  public String getBaseURI ()
  {
    Object uri = sequence.baseUriOfPos(ipos);
    return uri == null ? null : uri.toString();
  }

  public boolean isDefaultNamespace (String namespaceURI)
  {
    return ((NodeTree) sequence).posIsDefaultNamespace(ipos, namespaceURI);
  }

  public String lookupNamespaceURI (String prefix)
  {
    return ((NodeTree) sequence).posLookupNamespaceURI(ipos, prefix);
  }

  public String lookupPrefix (String namespaceURI)
  {
    return ((NodeTree) sequence).posLookupPrefix(ipos, namespaceURI);
  }

  /* #ifdef use:org.w3c.dom.Node */
  // public short compareDocumentPosition (Node other)
  //   throws DOMException
  // {
  //   if (! (other instanceof KNode))
  //     throw new DOMException(DOMException.NOT_SUPPORTED_ERR,
			     // "other Node is a "+other.getClass().getName());
  //   KNode n = (KNode) other;
  //   AbstractSequence nseq = n.sequence;
  //   return (short) (sequence == nseq ? nseq.compare(ipos, n.ipos)
		    // : (int) sequence.stableCompare(nseq));
  // }
  //   
  // public boolean isSameNode (Node node)
  // {
  //   if (! (node instanceof KNode))
  //     return false;
  //   KNode n = (KNode) node;
  //   if (sequence != n.sequence)
  //     return false;
  //   return sequence.equals(ipos, n.ipos);
  // }

  // public boolean isEqualNode (Node node)
  // {
  //   throw new UnsupportedOperationException("getAttributesisEqualNode not implemented yet");
  // }
  /* #endif */

  public String toString ()
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    XMLPrinter xp = new XMLPrinter(wr);
    ((NodeTree) sequence).consumeNext(ipos, xp);
    return wr.toString();
  }

  public Object getFeature (String feature, String version)
  {
    return null;
  }

  /* #ifdef JAXP-1.3 */
  // public Object setUserData (String key, Object data, UserDataHandler handler)
  // {
  //   throw new UnsupportedOperationException("setUserData not implemented yet");
  // }

  // public Object getUserData (String key)
  // {
  //   return null;
  // }
  /* #endif JAXP-1.3 */
}
