// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import org.w3c.dom.*;

public class KComment extends KCharacterData
  implements org.w3c.dom.Comment
{
  public KComment (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public short getNodeType () { return Node.COMMENT_NODE; }

  public String getNodeName()
  {
    return "#comment";
  }

}
