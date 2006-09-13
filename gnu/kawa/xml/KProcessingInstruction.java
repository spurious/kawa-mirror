// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import org.w3c.dom.*;

public class KProcessingInstruction extends KNode
  /* #ifdef use:org.w3c.dom.Node */
  // implements org.w3c.dom.ProcessingInstruction
  /* #endif */
{
  public KProcessingInstruction (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public short getNodeType () { return Node.PROCESSING_INSTRUCTION_NODE; }

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
