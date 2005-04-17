// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.xml.*;
import org.w3c.dom.*;

public class KCDATASection extends KText
  /* #ifdef use:org.w3c.dom.Node */
  // implements org.w3c.dom.CDATASection
  /* #endif */
{
  public KCDATASection (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public short getNodeType () { return Node.CDATA_SECTION_NODE; }

  public String getNodeName()
  {
    return "#cdata-section";
  }

  public String getData ()
  {
    return getNodeValue();
  }

  /** Non-optimized. */
  public int getLength ()
  {
    StringBuffer sbuf = new StringBuffer();
    NodeTree tlist = (NodeTree) sequence;
    tlist.stringValue(tlist.posToDataIndex(ipos), sbuf);
    return sbuf.length();
  }

}
