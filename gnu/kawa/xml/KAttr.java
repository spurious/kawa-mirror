// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import org.w3c.dom.*;

public class KAttr extends KNode
  /* #ifdef use:org.w3c.dom.Node */
  // implements org.w3c.dom.Attr
  /* #endif */
{
  public KAttr (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }
 
  public String getName ()
  {
    return sequence.getNextTypeName(ipos);
  }

  public short getNodeType () { return Node.ATTRIBUTE_NODE; }

  public String getValue ()
  {
    return getNodeValue();
  }

  /** Get attribute value as (typed) Object, rather than string. */
  public Object getObjectValue ()
  {
    return sequence.getPosNext(ipos+10);
  }

  /* #ifdef use:org.w3c.dom.Node */
  // public void setValue (String value)
  //   throws DOMException
  // {
  //   throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   // "setValue not supported");
  // }

  // public Node getParentNode()
  // {
  //   return null;
  // }

  // public Element  getOwnerElement ()
  // {
  //   return (Element) super.getParentNode();
  // }
  /* #endif */

  public boolean getSpecified ()
  {
    return true;
  }

  /* #ifdef JAXP-1.3 */
  // public TypeInfo getSchemaTypeInfo ()
  // {
  //   return null;
  // }

  // public boolean isId ()
  // {
  //   return false;
  // }
  /* #endif JAXP-1.3 */
}
