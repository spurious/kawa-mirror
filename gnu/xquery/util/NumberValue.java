// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.math.*;
import gnu.mapping.*;

public class NumberValue extends Procedure1
{
  public static final NumberValue numberValue = new NumberValue();

  public static Object numberValue (Object value)
  {
    if (value instanceof Number || value == Values.empty)
      return value;
    else
      {
	String str = StringValue.stringValue(value).trim();
	for (int i = str.length();  --i >= 0; )
	  {
	    char ch = str.charAt(i);
	    if (ch != '-' && ch != '+' && ! Character.isDigit(ch))
	      return new DFloNum(str);
	  }
	return IntNum.valueOf(str, 10);
      }
  }

  public Object apply1(Object arg)
  {
    return numberValue(arg);
  }
}
