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
	    TreeList tlist = (TreeList) pos.sequence;
	    tlist.stringValue(tlist.posToDataIndex(pos.ipos), sbuf);
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
	    int kind = tlist.getNextKind(index);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      stringValue(tlist.getPosNext(index), sbuf);
	    else
	      tlist.stringValue(tlist.posToDataIndex(index), sbuf);
	    index = tlist.nextPos(index);
	  }
      }
    else
      stringValue(node, sbuf);
    return sbuf.toString();
  }

  public static Object lowerCase (Object node)
  {
    if (node == Values.empty || node == null)
      return Values.empty;
    return stringValue(node).toLowerCase();
  }

  public static Object upperCase (Object node)
  {
    if (node == Values.empty)
      return Values.empty;
    return stringValue(node).toUpperCase();
  }

  public static Object substring (Object str, Object start)
  {
    if (str == Values.empty || start == Values.empty)
      return Values.empty;
    
    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;
     
    return str.toString().substring(i);
  }

  public static Object substring (Object str, Object start,Object length)
  {
    if (str == Values.empty || start == Values.empty
	|| length == Values.empty)
      return Values.empty;
    
    int i = ((Number) NumberValue.numberValue(start)).intValue() - 1;
    int len = ((Number) NumberValue.numberValue(length)).intValue();
     
    return str.toString().substring(i, i + len);
  }
}
