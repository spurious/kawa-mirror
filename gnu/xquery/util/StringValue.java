// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;

public class StringValue extends Procedure1
{
  public static final StringValue stringValue
  = new StringValue("string-value");
  public static final StringValue string = new StringValue("string");

  public StringValue(String name)
  {
    super(name);
  }

  public static String stringValue (Object node)
  {
    StringBuffer sbuf = new StringBuffer();
    stringValue(node, sbuf);
    return sbuf.toString();
  }

  public static void stringValue (Object node, StringBuffer sbuf)
  {
    if (node instanceof TreeList)
      {
	((TreeList) node).stringValue(0, sbuf);
	return;
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  {
	    ((TreeList) pos.sequence).stringValue(pos.ipos >> 1, sbuf);
	    return;
	  }
      }
    sbuf.append(node);
  }

  public Object apply1 (Object node)
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index << 1, null);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      stringValue(tlist.getNext(index << 1, null), sbuf);
	    else
	      tlist.stringValue(index, sbuf);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      stringValue(node, sbuf);
    return sbuf.toString();
  }
}
