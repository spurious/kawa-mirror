// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import org.w3c.dom.*;

public class KElement extends KNode
  implements org.w3c.dom.Element
{
  public KElement (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public short getNodeType () { return Node.ELEMENT_NODE; }

  public String getTagName ()
  {
    return sequence.getNextTypeName(ipos);
  }

  public String getNodeValue()
  {
    return null;
  }

  public boolean hasAttributes ()
  {
    return ((NodeTree) sequence).posHasAttributes(ipos);
  }

  /** Not implemented yet. */
  public String getAttribute (String name)
  {
    throw new UnsupportedOperationException("getAttribute not implemented yet");
  }

  /** Not implemented. */
   public void setAttribute (String name, String value)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setAttribute not supported");
  }

  /** Not implemented. */
  public void setIdAttribute (String name, boolean isId)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setIdAttribute not supported");
  }

  /** Not implemented. */
  public void setIdAttributeNS (String namespaceURI, String localName,
				boolean isId)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setIdAttributeNS not supported");
  }

  /** Not implemented. */
  public void setIdAttributeNode (Attr idAttr, boolean isId)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setIdAttributeNode not supported");
  }

  /** Not implemented. */
   public void removeAttribute (String name)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "removeAttribute not supported");
  }

  /** Not implemented yet. */
  public Attr getAttributeNode (String name)
  {
    throw new UnsupportedOperationException("getAttributeNode not implemented yet");
  }

  /** Not implemented. */
   public Attr setAttributeNode (Attr newAttr)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setAttributeNode not supported");
  }

  /** Not implemented. */
   public Attr removeAttributeNode (Attr oldAttr)
     throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "removeAttributeNode not supported");
  }

  /** Not implemented yet. */
  public String getAttributeNS (String namespaceURI, String localName)
  {
    throw new UnsupportedOperationException("getAttributeNS not implemented yet");
  }

  /** Not implemented. */
  public void setAttributeNS (String namespaceURI, String qualifiedName, 
			      String value) throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setAttributeNS not supported");
  }

  /** Not implemented. */
  public void removeAttributeNS (String namespaceURI, String localName)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "removeAttributeNS not supported");
  }

  /** Not implemented yet. */
  public Attr getAttributeNodeNS(String namespaceURI, String localName)
  {
    throw new UnsupportedOperationException("getAttributeNodeNS not implemented yet");
  }

  /** Not implemented. */
  public Attr setAttributeNodeNS (Attr newAttr)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setAttributeNodeNS not supported");
  }

  /** Not implemented yet. */
  public NodeList getElementsByTagNameNS(String namespaceURI, String localName)
  {
    throw new UnsupportedOperationException("getElementsByTagNameNS not implemented yet");
  }

  /** Not implemented yet. */
  public boolean hasAttribute (String name)
  {
    return getAttribute(name) != null;
  }

  /** Not implemented yet. */
  public boolean hasAttributeNS (String namespaceURI, String localName)
  {
    return getAttributeNS(namespaceURI, localName) != null;
  }

  /* #ifdef JAXP-1.3 */
  // public TypeInfo getSchemaTypeInfo ()
  // {
  //   return null;
  // }
  /* #endif JAXP-1.3 */
}
