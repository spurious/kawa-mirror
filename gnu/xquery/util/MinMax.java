// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;

public class MinMax extends Reduce
{
  public static final MinMax min = new MinMax("min", false);
  public static final MinMax max = new MinMax("max", true);

  boolean returnMax;

  public MinMax(String name, boolean returnMax)
  {
    super(name, null);
    this.returnMax = returnMax;
  }

  public Object combine (Object arg1, Object arg2)
  {
    if (arg1 == Values.empty)
      return arg2; // FIXME - verify that arg2 is comparable.
    int flags = returnMax ? Compare.TRUE_IF_GRT :  Compare.TRUE_IF_LSS;
    return Compare.apply(flags, arg1, arg2) ? arg1 : arg2;
  }
}
