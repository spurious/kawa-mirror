// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import org.w3c.dom.*;

public class KProcessingInstruction extends KNode
  implements org.w3c.dom.ProcessingInstruction
{
  public KProcessingInstruction (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public short getNodeType () { return Node.TEXT_NODE; }

  public String getNodeName()
  {
    return "#text";
  }

  public String getData ()
  {
    return getNodeValue();
  }

  public void setData(String data)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setData not supported");
  }

  public String getTarget ()
  {
    return ((NodeTree) sequence).posTarget(ipos);
  }
}
