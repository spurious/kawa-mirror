// Copyright (c) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.Numeric;
import gnu.kawa.xml.UntypedAtomic;

public class BooleanValue extends Procedure1
{
  public static final BooleanValue booleanValue = new BooleanValue();

  public static boolean booleanValue(Object value)
  {
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue();
    if (value instanceof Numeric)
      return ! ((Numeric) value).isZero();
    if (value instanceof Number)
      {
        double d = ((Number) value).doubleValue();
        return d != 0 && ! Double.isNaN(d);
      }
    if (value instanceof SeqPosition)
      return true;
    if (value instanceof String || value instanceof UntypedAtomic)
      return value.toString().length() > 0;
    if (value instanceof Values)
      {
	Values values = (Values) value;
	value = values.getPosNext(0);
	if (value == Sequence.eofValue)
	  return false;
	int next = values.nextDataIndex(0);
	if (next < 0)
	  return booleanValue(value);
      }
    return true;
  }

  public Object apply1(Object arg)
  {
    return booleanValue(arg) ? Boolean.TRUE : Boolean.FALSE;
  }
}
