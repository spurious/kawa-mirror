// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import org.w3c.dom.*;

public class KText extends KCharacterData
  implements org.w3c.dom.Text
{
  public KText (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public short getNodeType () { return Node.TEXT_NODE; }

  public String getNodeName()
  {
    return "#text";
  }

  public Text splitText(int offset)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "splitText not supported");
  }

  public boolean hasAttributes ()
  {
    return false;
  }
}
