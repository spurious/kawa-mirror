// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;

public class BooleanValue extends Procedure1
{
  public static final BooleanValue booleanValue = new BooleanValue();

  public static boolean booleanValue(Object value)
  {
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue();
    if (value instanceof SeqPosition)
      return true;
    if (value instanceof Values)
      {
	Values values = (Values) value;
	value = values.getNext(0, null);
	if (value == Sequence.eofValue)
	  return false;
	if (value instanceof SeqPosition
	    || value instanceof TreeList)
	  return true;
	int next = values.nextDataIndex(0);
	if (value instanceof Boolean && next < 0)
	  return ((Boolean) value).booleanValue();
      }
    else if (value instanceof TreeList)
      return ! ((TreeList) value).isEmpty();
    throw new Error("invalid boolean value");
  }

  public Object apply1(Object arg)
  {
    return booleanValue(arg) ? Boolean.TRUE : Boolean.FALSE;
  }
}
