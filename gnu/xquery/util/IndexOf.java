// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.math.IntNum;

public class IndexOf extends Procedure2
{
  public static final IndexOf indexOf = new IndexOf();

  public Object apply2(Object arg1, Object arg2)
  {
    return IntNum.make(arg1.toString().indexOf(arg2.toString()) + 1);
  }
}
