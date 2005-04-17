// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import org.w3c.dom.*;

public class KText extends KCharacterData
  /* #ifdef use:org.w3c.dom.Node */
  // implements org.w3c.dom.Text
  /* #endif */
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

  public String getWholeText ()
  {
    throw new UnsupportedOperationException("getWholeText not implemented yet");
  }

  public Text replaceWholeText (String content)
    throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "splitText not supported");
  }

  public boolean hasAttributes ()
  {
    return false;
  }

  public boolean isElementContentWhitespace ()
  {
    return false;
  }
}
