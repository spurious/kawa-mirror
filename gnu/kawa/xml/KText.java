// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import org.w3c.dom.*;

public class KText extends KNode
  implements org.w3c.dom.Text
{
  public KText (NodeTree seq, int ipos)
  {
    super(seq, ipos);
  }

  public short getNodeType () { return Node.ATTRIBUTE_NODE; }

  public String getNodeName()
  {
    return "#text";
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

  public void setData(String data)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "setData not supported");
  }

  public String substringData(int offset, int count)
    throws DOMException
  {
    String data = getData();
    if (offset < 0 || count < 0 || offset + count >= data.length())
      throw new DOMException(DOMException.INDEX_SIZE_ERR,
			     "invalid index to substringData");
    return data.substring(offset, count);
  }

  public void appendData (String data)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "appendData not supported");
  }

  public void insertData (int offset, String data)  throws DOMException
  {
    replaceData(offset, 0, data);
  }

  public void deleteData (int offset, int count)  throws DOMException
  {
    replaceData(offset, count, "");
  }

  public void replaceData (int offset, int count, String arg)  throws DOMException
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR,
			   "replaceData not supported");
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
