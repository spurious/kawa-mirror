// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.math.IntNum;

public class LastIndexOf extends Procedure2
{
  public static final LastIndexOf lastIndexOf = new LastIndexOf();

  public Object apply2(Object arg1, Object arg2)
  {
    return IntNum.make(arg1.toString().lastIndexOf(arg2.toString()) + 1);
  }
}
